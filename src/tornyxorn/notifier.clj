(ns tornyxorn.notifier
  (:require [clojure.core.async :refer [<! go-loop close!]]
            [com.rpl.specter :as sp]
            [com.rpl.specter.macros :refer [transform]]
            [immutant.web.async :as async]
            [cheshire.core :as json]
            [com.stuartsierra.component :as component]
            [clojure.tools.logging :as log]))

(defmulti notify
  "Notifies the appropriate users via their websocket connection, given a message."
  (fn [_ {:keys [msg/type]}] type))

(defmethod notify :msg/submit-api-key [_ {:keys [msg/ws player/api-key]}]
  (async/send! ws (json/encode {:type "submit-api-key"
                                :result "success"
                                :api-key api-key})))

(defmethod notify :msg/error [ws-map {:keys [msg/ws error/error] :as msg}]
  (if (= :error/invalid-api-key (:error/type error))
    (let [invalid-conns (filter (fn [[_ m]] (= (:api-key m) (:player/api-key error))) @ws-map)]
      (doseq [[ws _] invalid-conns]
        (async/send! ws (json/encode {:type "error"
                                      :error {:type "Invalid API key"
                                              :api-key (:player/api-key error)}}))))
    (log/error "Unhandled error:" msg)))

(def player-keys [:player/torn-id :player/xanax-taken :player/stat-enhancers-used
                  :player/refills :player/name :player/level])

(defn select-player-ws-props [player]
  (transform [sp/ALL sp/FIRST] name (select-keys player player-keys)) )

(defmethod notify :msg/unknown-player [ws-map {:keys [msg/resp] :as msg}]
  (log/debug "msg:" msg)
  (log/debug "ws-map:" @ws-map)
  (doseq [[ws _] (filter (fn [[_ {:keys [players]}]]
                           (contains? players (:player/torn-id resp)))
                         @ws-map)]
    (log/debug "Sending" resp)
    (async/send!
     ws
     (json/encode
      {:type "player"
       :player (select-player-ws-props resp)}))))

(defmethod notify :msg/known-players [_ {:keys [msg/players msg/ws]}]
  (async/send! ws (json/encode {:type "players" :players (mapv select-player-ws-props players)})))

(defrecord Notifier [notify-chan ws-map]
  component/Lifecycle
  (start [component]
    (go-loop [msg (<! notify-chan)]
      (when msg
        (log/debug "Notifier recieved:" msg)
        (notify ws-map msg)
        (recur (<! notify-chan))))
    component)
  (stop [component]
    (close! notify-chan)
    component))

(defn new-notifier [{:keys [notify-chan ws-map] :as config}]
  (map->Notifier config))
