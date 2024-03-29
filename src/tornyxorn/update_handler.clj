(ns tornyxorn.update-handler
  (:require [clojure.core.async :refer [<! >! >!! go-loop close! timeout]]
            [com.stuartsierra.component :as component]
            [com.rpl.specter :as sp]
            [com.rpl.specter.macros :refer [select]]
            [tornyxorn.db :as db]
            [tornyxorn.response-types :as rt]
            [tornyxorn.torn-api :as api]
            [clojure.tools.logging :as log]
            [datomic.api :as d]
            [clojure.string :as string]
            [environ.core :refer [env]]))

(defn log-string [msg]
  (str "Handling "
       (case (:msg/type msg)
         :msg/error "error"
         :msg/attacks "attacks"
         :msg/battle-stats (str "battle stats for " (:player/api-key msg))
         :msg/known-players (str "known players" #_(string/join ", " (map :player/torn-id (:msg/players msg))))
         :msg/unknown-player (str "unknown player " (-> msg :msg/resp :player/torn-id))
         :msg/submit-api-key (str "submitted API key: " (:player/api-key msg))
         (str "unknown message type: " (:msg/type msg)))))

(defmulti store-update
  "Stores an update from the API to the database."
  (fn [_ {:keys [resp/type]}] (:name type)))

(defmethod store-update :resp/basic-info [db {:keys [msg/resp]}]
  (db/add-player db resp))

(defmethod store-update :resp/player-info [db {:keys [msg/resp]}]
  (db/add-player-info db resp))

(defmethod store-update :resp/battle-stats [db {:keys [msg/resp player/torn-id] :as msg}]
  (db/update-battle-stats db (:player/torn-id (db/player-by-api-key db (:player/api-key msg)))
                          resp))

(defmethod store-update :resp/attacks [db {:keys [msg/resp]}]
  (db/add-attacks db resp))

(defmulti handle-update (fn [_ _ _ _ msg] (:msg/type msg)))

(def battle-stats-updates-needed (atom #{}))

(defmethod handle-update :msg/error
  [db _ notify-chan token-buckets {:keys [error/error] :as msg}]
  (case (:error/type error)
    :error/invalid-api-key
    (do
      (swap! battle-stats-updates-needed disj
             (:player/torn-id (db/player-by-api-key db (:player/api-key error))))
      (db/remove-api-key db (:player/api-key error))
      (api/del-bucket! token-buckets (:player/api-key error))
      (>!! notify-chan msg))

    (log/error "Unhandled error:" msg)))

(defmethod handle-update :msg/attacks
  [db req-chan notify-chan _ {:keys [msg/resp] :as msg}]
  ;; Identify players which need a battle stats update
  (let [new-ids (db/new-attacks db resp)
        attack-map (zipmap (map :attack/torn-id resp) resp)
        new-attacks (select-keys attack-map new-ids)
        players (->> new-attacks
                     (select [sp/MAP-VALS (sp/multi-path :attack/attacker :attack/defender)
                              (complement nil?) #(db/has-api-key? db %)])
                     (distinct)
                     (map (partial db/player-by-id db)))]
    (when-not (empty? new-attacks)
      ;; Give up waiting for battle stats update when a new attacks message arrives
      (reset! battle-stats-updates-needed #{})
      ;; Send messages requesting necessary battle stats updates
      (doseq [{:keys [player/torn-id player/api-key]} players]
        (swap! battle-stats-updates-needed conj torn-id)
        (>!! req-chan {:msg/type :msg/battle-stats
                       :player/torn-id torn-id
                       :player/api-key api-key}))
      ;; Poll every 50ms waiting for necessary battle stats updates to be processed
      (go-loop [_ (<! (timeout 50))]
        (if (empty? @battle-stats-updates-needed)
          (do (log/debug "Necessary battle stats aquired!")
              (store-update db msg)
              ;; Refresh difficulties for anyone interested in players
              (doseq [p players]
                (>! notify-chan {:msg/type :msg/unknown-player
                                 :msg/resp (into {} p)})))
          (recur (<! (timeout 50))))))))

(defmethod handle-update :msg/battle-stats
  [db _ notify-chan _ {:keys [player/api-key] :as msg}]
  (store-update db msg)
  (swap! battle-stats-updates-needed disj (:player/torn-id msg))
  #_(>!! notify-chan {:msg/type :msg/update-all :player/api-key api-key}))

(defmethod handle-update :msg/unknown-player
  [db _ notify-chan _ {:keys [msg/resp] :as msg}]
  (let [with-difficulties (update msg :msg/resp merge (db/estimate-stats db (:player/torn-id resp)))]
    (log/debug (:msg/resp with-difficulties))
    (store-update db with-difficulties)
    (>!! notify-chan with-difficulties)))

(defmethod handle-update :msg/known-players
  [_ _ notify-chan _ msg]
  (log/debug "Handling known players:" msg)
  (>!! notify-chan msg))

(defmethod handle-update :msg/submit-api-key
  [db req-chan notify-chan token-buckets {:keys [player/api-key msg/resp] :as msg}]
  (log/debug "Handling" msg)
  ;; HACK ALSO
  (if (contains? (into #{} (map #(Integer. %)) (string/split (env :faction-ids) #",")) (:player/faction resp))
    (do
      (let [player (assoc (:msg/resp msg) :player/api-key api-key)]
        ;; HACK
        ;; (log/info "Adding player" player)
        (db/add-player db (select-keys player [:player/torn-id :player/api-key]))
        (store-update db msg)
        (when (not= (env :api-key) api-key)
          (db/remove-temp-api-key db api-key)))
      (api/add-bucket! token-buckets api-key)
      ;; TODO: Pass battle-stats (and attacks?) update to req-chan
      (let [{:keys [player/torn-id]} resp]
        #_(>!! req-chan {:msg/type :msg/battle-stats
                         :player/torn-id torn-id
                         :player/api-key api-key})
        (>!! req-chan {:msg/type :msg/player-attacks-full
                       :player/torn-id torn-id
                       :player/api-key api-key})
        #_(>!! req-chan {:msg/type :msg/players
                         :msg/ids [torn-id]
                         :player/api-key api-key
                         :msg/ws (:msg/ws msg)}))
      (>!! notify-chan msg))
    (do
      (db/remove-api-key db api-key)
      (>!! notify-chan {:msg/type :msg/error
                        :player/api-key api-key
                        :error/error {:error/type :error/invalid-faction}}))))

(defrecord UpdateHandler [db req-chan update-chan notify-chan]
  component/Lifecycle
  (start [component]
    (go-loop [msg (<! update-chan)]
      (log/info (log-string msg))
      (log/debug "Handler received:" msg)
      (when msg
        (handle-update db req-chan notify-chan (-> component :torn-api :token-buckets) msg)
        (recur (<! update-chan))))
    component)
  (stop [component]
    (close! update-chan)
    component))

(defn new-update-handler [{:keys [req-chan update-chan notify-chan] :as config}]
  (map->UpdateHandler config))
