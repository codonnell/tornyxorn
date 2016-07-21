(ns tornyxorn.update-handler
  (:require [clojure.core.async :refer [<! >!! go-loop close! timeout]]
            [com.stuartsierra.component :as component]
            [com.rpl.specter :as sp]
            [com.rpl.specter.macros :refer [select]]
            [tornyxorn.db :as db]
            [tornyxorn.response-types :as rt]
            [tornyxorn.torn-api :as api]
            [clojure.tools.logging :as log]
            [datomic.api :as d]))

(defmulti store-update
  "Stores an update from the API to the database."
  (fn [_ {:keys [resp/type]}] (:name type)))

(defmethod store-update :resp/basic-info [db {:keys [msg/resp]}]
  (db/add-player db resp))

(defmethod store-update :resp/player-info [db {:keys [msg/resp]}]
  (db/add-player-info db resp))

(defmethod store-update :resp/battle-stats [db {:keys [msg/resp player/torn-id]}]
  (db/update-battle-stats db torn-id resp))

(defmethod store-update :resp/attacks [db {:keys [msg/resp]}]
  (db/add-attacks db resp))

(defmulti handle-update (fn [_ _ _ _ msg] (:msg/type msg)))

(def battle-stats-updates-needed (atom #{}))

(defmethod handle-update :msg/error
  [db _ notify-chan token-buckets {:keys [error/error] :as msg}]
  (case (:error/type error)
    :error/invalid-api-key (do
                             (log/warn "Invalid API key:" (:player/api-key error))
                             (swap! battle-stats-updates-needed disj
                                    (:player/torn-id (db/player-by-api-key db (:player/api-key error))))
                             (db/remove-api-key (:player/api-key error))
                             (api/del-bucket! token-buckets (:player/api-key error)))
    (log/error "Unhandled error:" msg)))

(defmethod handle-update :msg/attacks
  [db req-chan notify-chan _ {:keys [msg/resp] :as msg}]
  ;; Identify players which need a battle stats update
  (let [new-ids (db/new-attacks db resp)
        attack-map (zipmap (map :attack/torn-id resp) resp)
        new-attacks (select-keys attack-map new-ids)
        players (->> new-attacks
                     (select [sp/MAP-VALS (sp/multi-path :attack/attacker :attack/defender)
                              (sp/pred identity) #(db/has-api-key? db %)])
                     (distinct)
                     (map (partial db/player-by-id db)))]
    (reset! battle-stats-updates-needed #{})
    ;; Send messages requesting necessary battle stats updates
    (doseq [{:keys [player/torn-id player/api-key]} players]
      (swap! battle-stats-updates-needed conj torn-id)
      (>!! req-chan {:msg/type :msg/battle-stats
                     :player/torn-id torn-id
                     :player/api-key api-key}))
    ;; Poll every 500ms waiting for necessary battle stats updates to be processed
    (go-loop [_ (<! (timeout 500))]
      (if (empty? @battle-stats-updates-needed)
        (do (log/debug "Necessary battle stats aquired!")
            (store-update db msg))
        (recur (<! (timeout 500)))))))

(defmethod handle-update :msg/battle-stats
  [db _ _ _ msg]
  (store-update db msg)
  (swap! battle-stats-updates-needed disj (:player/torn-id msg)))

(defmethod handle-update :msg/unknown-player
  [db _ notify-chan _ {:keys [msg/resp] :as msg}]
  (store-update db msg)
  (>!! notify-chan msg))

(defmethod handle-update :msg/known-players
  [_ _ notify-chan _ msg]
  (>!! notify-chan msg))

(defmethod handle-update :msg/submit-api-key
  [db req-chan notify-chan token-buckets {:keys [player/api-key player/resp] :as msg}]
  (log/debug "Handling" msg)
  (let [player (assoc (:msg/resp msg) :player/api-key api-key)]
    (log/info "Adding player" player)
    (db/add-player db player))
  (api/add-bucket! token-buckets api-key)
  ;; TODO: Pass battle-stats (and attacks?) update to req-chan
  ;; (>!! req-chan {:msg/type :msg/battle-stats
  ;;                :player/torn-id (-> msg :msg/resp :player/torn-id)
  ;;                :player/api-key api-key})
  (>!! notify-chan msg))


(defrecord UpdateHandler [db req-chan update-chan notify-chan]
  component/Lifecycle
  (start [component]
    (go-loop [msg (<! update-chan)]
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
