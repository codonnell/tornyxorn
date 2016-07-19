(ns tornyxorn.update-creator
  (:require [com.stuartsierra.component :as component]
            [immutant.web.async :as async]
            [cheshire.core :as json]
            [tornyxorn.db :as db]
            [tornyxorn.torn-api :as api]
            [clojure.tools.logging :as log]
            [clojure.core.async :refer [go-loop <! >! close!]]))

(defmulti create-update
  "Processes an update request message and returns a vector of updates and
  update requests, depending on which updates need more information."
  (fn [_ _ msg] (get-in msg [:msg/type])))

(defmethod create-update :msg/submit-api-key
  [db token-buckets {:keys [player/api-key] :as msg}]
  (log/info "API key" api-key "submitted")
  (api/add-bucket! token-buckets api-key)
  (db/add-api-key db api-key)
  [msg])

(defn keep-or-identity [f coll]
  "Returns two vectors, the first containing (f item) for all items where (f
  item) is truthy. The second contains the items for which (f item) is not
  truthy."
  (reduce (fn [[coll1 coll2] x]
            (if-let [y (f x)]
              [(conj coll1 y) coll2]
              [coll1 (conj coll2 x)]))
          [[] []] coll))

(defmethod create-update :msg/players
  [db _ {:keys [msg/ids] :as msg}]
  (let [groups (group-by #(db/has-player-info? db %) ids)]
    [(-> msg
         (assoc :msg/type :msg/known-players, :msg/players (mapv #(db/player-by-id db %) (groups true)))
         (dissoc :msg/ids))
     (assoc msg :msg/type :msg/unknown-players, :msg/ids (groups false))]))

;; This should never be called.
(defmethod create-update :default
  [_ _ {:keys [msg/ws] :as msg}]
  (log/error "Invalid input:" (dissoc msg :msg/ws))
  (async/send! ws (str "Invalid input: " (dissoc msg :msg/ws)))
  [])

(def update-dest
  {:msg/known-players :update-handler
   :msg/unknown-players :torn-api
   :msg/submit-api-key :torn-api})

(defrecord UpdateCreator [db req-chan api-chan update-chan]
  component/Lifecycle
  (start [component]
    (go-loop [{:keys [msg/ws] :as msg} (<! req-chan)]
      (log/debug "Websocket message:" msg)
      ;; Updates requiring info from the torn api are sent there and updates
      ;; with all necessary info are sent to the update handler
      (doseq [msg (create-update db (-> component :torn-api :token-buckets) msg)]
        (condp = (-> msg :msg/type update-dest)
          :update-handler (>! update-chan msg)
          :torn-api (>! api-chan msg)))
      (when-let [msg (<! req-chan)]
        (recur msg)))
    component)
  (stop [component]
    (close! req-chan)
    component))

(defn new-update-creator [{:keys [req-chan api-chan update-chan token-buckets] :as config}]
  (map->UpdateCreator config))
