(ns tornyxorn.torn-api
  (:refer-clojure :exclude [error-handler])
  (:require [org.httpkit.client :as http]
            [clojure.core.async :refer [go go-loop >! <! close! >!! chan dropping-buffer alts!
                                        timeout]]
            [clojure.string :as string]
            [cheshire.core :as json]
            [tornyxorn.db :as db]
            [tornyxorn.response-types :as rt]
            [com.stuartsierra.component :as component]
            [tornyxorn.log :as log]
            [clojure.set :as set]))

(defn wrap-errors [handler]
  (fn [{:keys [msg/req msg/resp] :as msg}]
    (if-let [err-code (get-in resp [:error :code])]
      (cond (or (= 1 err-code) (= 2 err-code))
            (do (log/error "Invalid API key" (:player/api-key req))
                (assoc msg
                       :msg/type :msg/error
                       :resp/type :resp/error
                       :error/error {:error/type :error/invalid-api-key
                                     :player/api-key (:player/api-key req)}))
            ;; (throw (ex-info "Invalid API key" {:player/api-key (:player/api-key req)
            ;;                                    :error/type :error/invalid-api-key}))

            (= 5 err-code)
            (do (log/error "Exceeded rate limit for" (:player/api-key req))
                (assoc msg
                       :msg/type :msg/error
                       :resp/type :resp/error
                       :error/error {:error/type :error/too-many-requests
                                     :player/api-key (:player/api-key req)}))
            ;; (throw (ex-info "Too many requests" {:player/api-key (:player/api-key req)
            ;;                                      :error/type :too-many-requests}))

            (= 8 err-code)
            (do (log/error "Temporary IP ban")
                (assoc msg
                       :msg/type :msg/error
                       :resp/type :resp/error
                       :error/error {:error/type :error/temp-ip-ban}))
            ;; (throw (ex-info "Temporary IP ban" {}))

            :default (throw (ex-info "Unhandled error" msg)))
      (handler msg))))

(defn wrap-json [handler]
  "Changes the response to be a json-decoded map of the response body"
  (fn [msg]
    (handler
     (update msg :msg/resp
             #(-> % :body (json/decode true))))))

(defn split-on-resp-types [handler]
  "Returns a list of the result of calling handler on each sub-response."
  (fn [{:keys [msg/resp] :as msg}]
    (let [resps (->> rt/resp-types
                     (filter #(rt/identify % resp))
                     (map (fn [rtype]
                            [(rt/extract rtype resp)
                             rtype])))]
      (map handler
           (map #(assoc msg :msg/resp (first %) :resp/type (second %)) resps)))))

(defn wrap-coerce [handler]
  "Coerces the response body according to its type"
  (fn [{:keys [resp/type] :as msg}]
    (handler (update msg :msg/resp #(rt/coerce type %)))))

(defn wrap-handler [handler]
  "Note that the last middleware in this form will be applied first."
  (-> handler
      wrap-coerce
      split-on-resp-types
      wrap-errors
      wrap-json))

(def resp-handler (wrap-handler #(do (log/debug %) %)))

(defn error-handler [token-buckets]
  "Delays further requests in the case of an api key exceeding the rate limit or
  a temporary IP ban."
  (fn [msg]
    (if-not (= :resp/error (:resp/type msg))
      msg
      (let [error (:error/error msg)]
        (case (:error/type error)
          :error/too-many-requests (do (go (dotimes [_ 20]
                                             (<! (get @token-buckets (:player/api-key error)))))
                                       nil)
          :error/temp-ip-ban (doseq [[_ bucket] @token-buckets]
                               (go (dotimes [_ 20] (<! bucket))))
          msg)))))

(defn query-url
  ([{:keys [endpoint api-key selections id]}]
   (cond (empty? api-key) (throw (ex-info "api-key cannot be empty" {}))
         (empty? selections) (throw (ex-info "selections cannot be empty" {}))
         (empty? endpoint) (throw (ex-info "must query an endpoint" {}))
         :default (format "https://api.torn.com/%s/%s?selections=%s&key=%s"
                          endpoint
                          (if id (str id) "")
                          (string/join "," selections)
                          api-key))))


(defn api-call [update-chan token-buckets msg]
  "Executes the request stored in :msg/req, stores the parsed response in
  :msg/resp, and puts the message onto update-chan. Asynchronous, so returns
  immediately."
  (go
    (log/debug "Before token:" msg)
    (<! (get @token-buckets (-> msg :msg/req :api-key)))
    (log/debug "After token:" msg)
    (http/get (query-url (:msg/req msg)) {:timeout 1000}
              (fn [resp] (->> resp
                              (assoc msg :msg/resp)
                              (resp-handler)
                              (keep (error-handler token-buckets))
                              ((fn [msgs] (log/debug "Responses:" msgs) msgs))
                              ((fn [msgs]
                                 (doseq [msg msgs]
                                   ;; (log/info "In doseq:" msg)
                                   (>!! update-chan msg)))))))))

(defn very-lazy-apply-concat
  "lazier than (apply concat seqs)... evaluates nothing at construction time"
  ([colls] (very-lazy-apply-concat (lazy-seq (first colls)) (lazy-seq (next colls))))
  ([coll colls]
   (lazy-seq
    (cond
      (and (empty? coll) (empty? colls)) nil
      (empty? coll) (very-lazy-apply-concat (first colls) (next colls))
      :else (cons (first coll) (very-lazy-apply-concat (next coll) colls))))))

(defmulti create-reqs
  "Given an update request, creates the corresponding api requests."
  (fn [update-req _] (:msg/type update-req)))

(defrecord Request [endpoint id selections api-key])

(defmethod create-reqs :msg/battle-stats
  [{:keys [player/api-key :player/torn-id] :as update-req} api-key-seq]
  [[(assoc update-req :msg/req
           (map->Request
            {:endpoint "user" :selections ["battlestats"] :api-key api-key :id torn-id}))]
   api-key-seq])

(defmethod create-reqs :msg/submit-api-key [update-req api-key-seq]
  [[(assoc update-req :msg/req
           (map->Request
            {:endpoint "user" :selections ["basic"] :api-key (:player/api-key update-req)}))]
   api-key-seq])

(defmethod create-reqs :msg/unknown-players [update-req api-key-seq]
  (let [reqs (map (fn [id api-key]
                    {:msg/type :msg/unknown-player
                     :player/torn-id id
                     :msg/req (map->Request
                               {:endpoint "user"
                                :selections ["profile" "personalstats"]
                                :id id
                                :api-key api-key})})
                  (:msg/ids update-req)
                  api-key-seq)]
    [reqs (drop (count reqs) api-key-seq)]))

(defmethod create-reqs :msg/player-attacks
  [{:keys [player/torn-id player/api-key] :as update-req} api-key-seq]
  [[(assoc update-req
           :msg/req (map->Request {:endpoint "user"
                                   :selections ["attacks"]
                                   :id torn-id
                                   :api-key api-key})
           :msg/type :msg/attacks)]
   api-key-seq])

(defmethod create-reqs :msg/faction-attacks
  [{:keys [faction/torn-id player/api-key] :as update-req} api-key-seq]
  [[(assoc update-req
           :msg/req (map->Request {:endpoint "faction"
                                   :selections ["attacks"]
                                   :id torn-id
                                   :api-key api-key})
           :msg/type :msg/attacks)]
   api-key-seq])

(defn new-bucket []
  (let [c (chan (dropping-buffer 10))]
    (dotimes [_ 5] (>!! c :token))
    c))

(defn add-bucket! [token-buckets api-key]
  (when-not (contains? @token-buckets api-key)
    (swap! token-buckets assoc api-key (new-bucket))))

(defn del-bucket! [token-buckets api-key]
  (when (contains? @token-buckets api-key)
    (close! (get @token-buckets api-key))
    (swap! token-buckets dissoc api-key)))

(defn update-buckets [db token-buckets]
  (let [active-keys (set (db/api-keys db))
        bucket-keys (set (keys @token-buckets))
        new-keys (set/difference active-keys bucket-keys)
        expired-keys (set/difference bucket-keys active-keys)]
    (swap! token-buckets
           (fn [m]
             (as-> m m
               (reduce (fn [m k] (close! (m k)) (dissoc m k)) m expired-keys)
               (reduce (fn [m k] (assoc m k (new-bucket))) m new-keys))))))

(defn continuously-fill-buckets [db token-buckets finish-chan]
  (go-loop []
    (let [[_ c] (alts! [(timeout 6000) finish-chan])]
      (when (not= c finish-chan)
        (update-buckets db token-buckets)
        (doseq [[_ bucket] @token-buckets]
          (>! bucket :token))
        (recur)))))

(defrecord TornApi [db api-chan update-chan token-buckets finish-chan]
  component/Lifecycle
  (start [component]
    (let [token-buckets (atom {})
          finish-chan (chan)]
      (continuously-fill-buckets db token-buckets finish-chan)
      (go-loop [api-key-seq (very-lazy-apply-concat (repeatedly #(db/api-keys db)))
                update-req (<! api-chan)]
        (log/debug "Update request:" update-req)
        (when update-req
          (let [[reqs tail-seq] (create-reqs update-req api-key-seq)]
            (doseq [req reqs]
              (log/debug "Executing request" req)
              (api-call update-chan token-buckets req))
            (recur tail-seq (<! api-chan)))))
      (assoc component :token-buckets token-buckets :finish-chan finish-chan)))
  (stop [component]
    (close! api-chan)
    (close! finish-chan)
    component))

(defn new-torn-api [{:keys [db api-chan update-chan] :as config}]
  (map->TornApi config))

