(ns tornyxorn.update-creator
  (:require [com.stuartsierra.component :as component]
            [immutant.web.async :as async]
            [cheshire.core :as json]
            [clj-time.core :as t]
            [clj-time.coerce :refer [from-date]]
            [tornyxorn.db :as db]
            [tornyxorn.torn-api :as api]
            [clojure.tools.logging :as log]
            [clojure.core.async :refer [go-loop <! >! close! alts! timeout chan]]))

(defmulti create-update
  "Processes an update request message and returns a vector of updates and
  update requests, depending on which updates need more information."
  (fn [_ _ msg] (get-in msg [:msg/type])))

(defmethod create-update :msg/battle-stats
  [_ _ msg]
  [msg])

(defmethod create-update :msg/submit-api-key
  [db token-buckets {:keys [player/api-key] :as msg}]
  (log/info "API key" api-key "submitted")
  (api/add-bucket! token-buckets api-key)
  (db/add-api-key db api-key)
  [msg])

(defn up-to-date-info? [db torn-id]
  (and (db/has-player-info? db torn-id)
       (let [update-time (from-date (:player/last-player-info-update (db/player-by-id db torn-id)))]
         (t/after? update-time (t/ago (t/months 1))))))

(defmethod create-update :msg/players
  [db _ {:keys [msg/ids] :as msg}]
  (let [groups (group-by #(up-to-date-info? db %) ids)]
    [(-> msg
         (assoc :msg/type :msg/known-players, :msg/players (mapv #(db/player-by-id db %) (groups true)))
         (dissoc :msg/ids))
     (assoc msg :msg/type :msg/unknown-players, :msg/ids (groups false))]))

(defmethod create-update :msg/player-attacks
  [_ _ msg]
  [msg])

(defmethod create-update :default
  [_ _ {:keys [msg/ws] :as msg}]
  ;; msg should only be nil when component is shutting down
  (when-not (nil? msg)
    (log/error "Invalid input:" (dissoc msg :msg/ws))
    (async/send! ws (str "Invalid input: " (dissoc msg :msg/ws))))
  [])

(def update-dest
  {:msg/known-players :update-handler
   :msg/player-attacks :torn-api
   :msg/battle-stats :torn-api
   :msg/unknown-players :torn-api
   :msg/submit-api-key :torn-api})

(def faction-attack-msg
  (memoize
   (fn [faction-id api-key]
     {:msg/type :msg/faction-attacks
      :player/api-key api-key
      :faction/torn-id faction-id})))

(defn continuously-update-faction-attacks [db api-chan token-buckets faction-id api-key finish-chan]
  (db/add-api-key db api-key)
  (api/add-bucket! token-buckets api-key)
  (go-loop []
    (let [[_ c] (alts! [(timeout 6000) finish-chan])]
      (when (not= c finish-chan)
        ;; Faction attack update api key needs higher throughput
        (>! (@token-buckets api-key) :token)
        (>! api-chan (faction-attack-msg faction-id api-key))
        (recur)))))

(defrecord UpdateCreator [db req-chan api-chan update-chan faction-id api-key finish-chan]
  component/Lifecycle
  (start [component]
    (let [finish-chan (chan)
          token-buckets (-> component :torn-api :token-buckets)]
      (continuously-update-faction-attacks db api-chan token-buckets faction-id api-key finish-chan)
      (go-loop [msg (<! req-chan)]
        (log/debug "Websocket message:" msg)
        ;; Updates requiring info from the torn api are sent there and updates
        ;; with all necessary info are sent to the update handler
        (doseq [msg (create-update db token-buckets msg)]
          (condp = (-> msg :msg/type update-dest)
            :update-handler (>! update-chan msg)
            :torn-api (>! api-chan msg)))
        (when-let [msg (<! req-chan)]
          (recur msg)))
      (assoc component :finish-chan finish-chan)))
  (stop [component]
    (close! req-chan)
    (close! finish-chan)
    component))

(defn new-update-creator [config]
  (map->UpdateCreator config))
