(ns tornyxorn.update-creator
  (:require [com.stuartsierra.component :as component]
            [immutant.web.async :as async]
            [cheshire.core :as json]
            [clj-time.core :as t]
            [clj-time.coerce :refer [from-date]]
            [tornyxorn.db :as db]
            [tornyxorn.torn-api :as api]
            [tornyxorn.util :refer [do-every]]
            [clojure.tools.logging :as log]
            [clojure.string :as string]
            [clojure.core.async :refer [go-loop <! >!! >! close! alts! timeout chan]]))

(defn log-string [msg]
  (str "Updating "
       (case (:msg/type msg)
         :msg/battle-stats (str "battle stats for " (:player/torn-id msg))
         :msg/submit-api-key (str "api key " (:player/api-key msg))
         :msg/players (str "players " (string/join ", " (:msg/ids msg)))
         :msg/player-attacks (str "attacks for " (:player/torn-id msg))
         (str "unexpected message: " msg))))

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
    (remove (fn [m] (and (empty? (:msg/players m)) (empty? (:msg/ids m))))
            [(-> msg
                 (assoc :msg/type :msg/known-players, :msg/players (mapv #(db/player-by-id db %)
                                                                         (groups true)))
                 (dissoc :msg/ids))
             (assoc msg :msg/type :msg/unknown-players, :msg/ids (into [] (groups false)))])))

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
  (do-every 6000 finish-chan
            (fn []
              (>!! (@token-buckets api-key) :token)
              (>!! api-chan (faction-attack-msg faction-id api-key)))))

(defn continuously-update-players [db api-chan finish-chan]
  "Updates the stalest player info every 30 seconds, one player per api key in
  the system."
  (do-every 30000 finish-chan
            (fn []
              (let [ps (db/stale-players db (count (db/api-keys db)))]
                (when-not (empty? ps)
                  (>!! api-chan {:msg/type :msg/unknown-players
                                 :msg/ids (map :player/torn-id ps)}))))))

(defrecord UpdateCreator [db req-chan api-chan update-chan faction-id api-key
                          finish-faction finish-players]
  component/Lifecycle
  (start [component]
    (let [finish-faction (chan)
          finish-players (chan)
          token-buckets (-> component :torn-api :token-buckets)]
      (continuously-update-faction-attacks db api-chan token-buckets faction-id api-key finish-faction)
      (continuously-update-players db api-chan finish-players)
      (go-loop [msg (<! req-chan)]
        (log/info (log-string msg))
        (log/debug "Websocket message:" msg)
        ;; Updates requiring info from the torn api are sent there and updates
        ;; with all necessary info are sent to the update handler
        (doseq [msg (create-update db token-buckets msg)]
          (condp = (-> msg :msg/type update-dest)
            :update-handler (>! update-chan msg)
            :torn-api (>! api-chan msg)))
        (when-let [msg (<! req-chan)]
          (recur msg)))
      (assoc component :finish-faction finish-faction :finish-players finish-players)))
  (stop [component]
    (close! req-chan)
    (close! finish-faction)
    (close! finish-players)
    component))

(defn new-update-creator [config]
  (map->UpdateCreator config))
