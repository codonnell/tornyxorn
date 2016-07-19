(ns tornyxorn.update-handler
  (:require [clojure.core.async :refer [<! >!! go-loop close!]]
            [com.stuartsierra.component :as component]
            [tornyxorn.db :as db]
            [tornyxorn.response-types :as rt]
            [tornyxorn.torn-api :as api]
            [clojure.tools.logging :as log]))

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

(defmethod handle-update :msg/error
  [db _ notify-chan token-buckets {:keys [error/error] :as msg}]
  (case (:error/type error)
    :error/invalid-api-key (do (db/remove-api-key (:player/api-key error))
                               (api/del-bucket! token-buckets (:player/api-key error)))
    (log/error "Unhandled error:" msg)))

(defmethod handle-update :msg/faction-attacks
  [db req-chan notify-chan _ msg]
  (store-update db msg))

(defmethod handle-update :msg/battle-stats
  [db _ _ _ msg]
  (store-update db msg))

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
  (>!! req-chan {:msg/type :msg/battle-stats
                 :player/torn-id (-> msg :msg/resp :player/torn-id)
                 :player/api-key api-key})
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
