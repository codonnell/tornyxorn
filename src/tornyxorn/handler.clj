(ns tornyxorn.handler
  (:require [immutant.web :as web]
            [immutant.web.async :as async]
            [cheshire.core :as json]
            [clojure.tools.logging :as log]
            [clojure.set :refer [rename-keys]]
            [clojure.spec :as s]
            [clojure.core.async :refer [>!! close! go-loop chan alts! timeout]]
            [tornyxorn.spec :as spec]
            [com.stuartsierra.component :as component]
            [tornyxorn.db :as db]))

(def conns (atom #{}))

(def msg-keymap
  {:type :msg/type
   :ws :msg/ws
   :ids :msg/ids
   :api-key :player/api-key})

(def msg-types
  {"players" :msg/players
   "submit-api-key" :msg/submit-api-key
   "pong" :msg/pong})

(defn parse-msg [msg]
  "Properly namespaces the keys in the message and converts the message type to a keyword."
  (let [m (s/conform :ws-msg/msg msg)]
    (if (= m ::s/invalid)
      (throw (ex-info "Invalid message format" (s/explain-data :ws-msg/msg msg)))
      (-> m
          (rename-keys msg-keymap)
          (update :msg/type msg-types)))))

;; Spec for parse-msg
(s/fdef parse-msg
        :args (s/cat :msg :ws-msg/msg)
        :ret :msg/msg
        :fn (s/and #(= (-> % :args :msg :type msg-types) (-> % :ret :msg/type))
                   #(= (-> % :args :msg :ids) (-> % :ret :msg/ids))
                   #(= (-> % :args :msg :api-key) (-> % :ret :player/api-key))))

(defn continuously-ping-ws
  "Ping all websocket connections every 30 seconds to keep connections alive."
  [ws-map finish-chan]
  (go-loop []
    (let [[_ c] (alts! [(timeout 30000) finish-chan])]
      (when-not (= c finish-chan)
        (doseq [[ws _] @ws-map]
          (async/send! ws (json/encode {:type "ping"})))
        (recur)))))

(defn app [db req-chan ws-map]
  (fn [request]
    (async/as-channel
     request
     {:on-open (fn [ch]
                 (log/info "Client connected.")
                 (swap! ws-map assoc ch {})
                 (swap! conns conj ch))
      :on-error (fn [ch e]
                  (log/error "Websocket error:" e))
      :on-message (fn [ch msg]
                    (log/info "Received message:" msg)
                    ;; Parse message
                    (let [parsed-msg (try (parse-msg (assoc (json/decode msg true) :ws ch))
                                          (catch clojure.lang.ExceptionInfo e
                                            (log/error "Error parsing websocket message:" (str (ex-data e)))
                                            (async/send! ch (json/encode {:error "Invalid message format"
                                                                          :data (str (ex-data e))}))
                                            nil))]
                      (when-let [{:keys [player/api-key msg/type msg/ids] :as parsed-msg} parsed-msg]
                        ;; Do not pass along submit-api-key message when api key
                        ;; already exists. Immediately send response.
                        (cond
                          (and (= :msg/submit-api-key type)
                               (:player/strength (db/player-by-api-key db api-key)))
                          (async/send! ch
                                       (json/encode {:type "submit-api-key"
                                                     :result "success"
                                                     :api-key api-key}))
                          ;; Ignore pongs
                          (= :msg/pong type) nil
                          ;; Pass along all other messages.
                          :else
                          (do
                            (swap! ws-map update ch #(assoc % :player/api-key (:player/api-key parsed-msg)))
                            (when (= :msg/players (:msg/type parsed-msg))
                              (swap! ws-map update ch #(assoc % :players (set (:msg/ids parsed-msg)))))
                            (>!! req-chan parsed-msg))))))
      :on-close (fn [ch {:keys [code reason]}]
                  (log/info "Client disconnected." ch)
                  (swap! conns disj ch))
      })))

(defrecord Handler [handler db req-chan ws-map finish-pings]
  component/Lifecycle
  (start [component]
    (let [finish-pings (chan)]
      (continuously-ping-ws ws-map finish-pings)
      (assoc component :handler (app db req-chan ws-map) :finish-pings finish-pings)))
  (stop [component]
    (close! finish-pings)
    (assoc component :handler nil :finish-pings nil)))

(defn new-handler [{:keys [db req-chan ws-map] :as config}]
  (map->Handler config))
