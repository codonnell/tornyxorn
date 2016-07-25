(ns tornyxorn.notifier
  (:require [clojure.core.async :refer [<! go-loop close! timeout >! alts! chan >!!]]
            [com.rpl.specter :as sp]
            [com.rpl.specter.macros :refer [transform]]
            [immutant.web.async :as async]
            [cheshire.core :as json]
            [com.stuartsierra.component :as component]
            [clojure.tools.logging :as log]
            [tornyxorn.db :as db]
            [clojure.string :as string]))

(defn batch [in out max-time max-count]
  (let [lim-1 (dec max-count)]
    (go-loop [buf {} t (timeout max-time)]
      (let [[v p] (alts! [in t])]
        (cond
          (and (= p t) (not (empty? buf)))
          (do
            (log/info "buf:" buf)
            (>! out buf)
            (recur {} (timeout max-time)))

          (= p t)
          (recur {} (timeout max-time))

          (nil? v)
          (when-not (empty? buf)
            (>! out buf))

          (== (count buf) lim-1)
          (do
            (>! out (assoc buf (get v "torn-id") v))
            (recur {} (timeout max-time)))

          :else
          (recur (assoc buf (get v "torn-id") v) t))))))

(defn player-update-sink [ws]
  (let [in-c (chan 1)]
    (go-loop []
      (when-some [ps (<! in-c)]
        (log/info "sink:" ps)
        (async/send! ws (json/encode {:type "players"
                                      :players (vals ps)}))
        (recur)))
    in-c))

(defn notify-chan
  "Returns a channel that automatically batches and sends players messages it
  receives to a websocket connection."
  [ws]
  (let [in-c (chan 1)
        out-c (player-update-sink ws)]
    (batch in-c out-c 500 50)
    in-c))

(defn log-string [msg]
  (str "Notifying of "
       (case (:msg/type msg)
         :msg/submit-api-key (str "adding API key " (:player/api-key msg))
         :msg/error (str "invalid API key: " (-> msg :error/error :player/api-key))
         :msg/unknown-player (str "new player info for " (-> msg :msg/resp :player/torn-id))
         :msg/known-players (str "known player info for "
                                 (string/join ", " (map :player/torn-id (:msg/players msg))))
         (str "unknown msg type: " (:msg/type msg)))))

(defmulti notify
  "Notifies the appropriate users via their websocket connection, given a message."
  (fn [_ _ {:keys [msg/type]}] type))

(defmethod notify :msg/submit-api-key [_ _ {:keys [msg/ws player/api-key]}]
  (async/send! ws (json/encode {:type "submit-api-key"
                                :result "success"
                                :api-key api-key})))

(defmethod notify :msg/error [_ ws-map {:keys [msg/ws error/error] :as msg}]
  (if (= :error/invalid-api-key (:error/type error))
    (let [invalid-conns (filter (fn [[_ m]] (= (:api-key m) (:player/api-key error))) @ws-map)]
      (doseq [[ws _] invalid-conns]
        (async/send! ws (json/encode {:type "error"
                                      :error {:type "Invalid API key"
                                              :api-key (:player/api-key error)}}))))
    (log/error "Unhandled error:" msg)))

(def player-keys [:player/torn-id :player/xanax-taken :player/stat-enhancers-used
                  :player/refills :player/name :player/level :difficulty])

(defn select-player-ws-props [player]
  (transform [sp/ALL sp/FIRST] name (select-keys player player-keys)) )

(defn add-difficulty [db attacker-key {:keys [player/torn-id] :as player}]
  (log/debug (type player))
  (assoc player :difficulty (db/difficulty db (db/player-by-api-key db attacker-key) torn-id)))

(defmethod notify :msg/unknown-player [db ws-map {:keys [msg/resp] :as msg}]
  (log/debug "msg:" msg)
  (log/debug "ws-map:" @ws-map)
  (doseq [[ws {:keys [player/api-key out-c]}] (filter (fn [[_ {:keys [players]}]]
                                                        (contains? players (:player/torn-id resp)))
                                                      @ws-map)]
    (log/debug "Sending" resp)
    (>!! out-c (->> resp (add-difficulty db api-key) (select-player-ws-props)))))

(defmethod notify :msg/known-players [db ws-map {:keys [msg/players msg/ws player/api-key]}]
  (log/info "Players:" players)
  (doseq [p players]
    (->> p
         (into {}) ;; convert datomic.query.EntityMap to PersistentHashMap
         (add-difficulty db api-key)
         select-player-ws-props
         (>!! (get-in @ws-map [ws :out-c])))))

(defrecord Notifier [db notify-chan ws-map]
  component/Lifecycle
  (start [component]
    (go-loop [msg (<! notify-chan)]
      (when msg
        (log/info (log-string msg))
        (log/debug "Notifier recieved:" msg)
        (notify db ws-map msg)
        (recur (<! notify-chan))))
    component)
  (stop [component]
    (close! notify-chan)
    component))

(defn new-notifier [{:keys [db notify-chan ws-map] :as config}]
  (map->Notifier config))

