(ns tornyxorn.handler
  (:require [immutant.web :as web]
            [immutant.web.async :as async]
            [cheshire.core :as json]
            [tornyxorn.log :as log]
            [clojure.core.async :refer [>!!]]
            [clojure.set :refer [rename-keys]]
            [clojure.spec :as s]
            [tornyxorn.spec :as spec]
            [com.stuartsierra.component :as component]))

(defrecord Handler [handler]
  component/Lifecycle
  (start [component]
    (assoc component :handler handler))
  (stop [component]
    (assoc component :handler nil)))

(def conns (atom #{}))

(def msg-keymap
  {:type :msg/type
   :ws :msg/ws
   :ids :msg/ids
   :api-key :player/api-key})

(def msg-types
  {"players" :msg/players
   "submit-api-key" :msg/submit-api-key})

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

(defn app [db req-chan ws-map]
  (fn [request]
    (async/as-channel
     request
     {:on-open (fn [ch]
                 (log/info "Client connected.")
                 (swap! ws-map assoc ch {})
                 (swap! conns conj ch))
      :on-error (fn [ch e]
                  (log/error "Websocket error!"))
      :on-message (fn [ch msg]
                    (log/info "Received message:" msg)
                    (let [parsed-msg (try (parse-msg (assoc (json/decode msg true) :ws ch))
                                          (catch clojure.lang.ExceptionInfo e
                                            (log/error "Error parsing websocket message:" (str (ex-data e)))
                                            (async/send! ch (json/encode {:error "Invalid message format"
                                                                          :data (ex-data e)}))
                                            nil))]
                      (when parsed-msg
                        (swap! ws-map update ch #(assoc % :player/api-key (:player/api-key parsed-msg)))
                        (when (= :msg/players (:msg/type parsed-msg))
                          (swap! ws-map update ch #(assoc % :players (set (:msg/ids parsed-msg)))))
                        (>!! req-chan parsed-msg))))
      :on-close (fn [ch {:keys [code reason]}]
                  (log/info "Client disconnected." ch)
                  (swap! conns disj ch))
      })))

(defn new-handler [{:keys [db req-chan ws-map]}]
  (->Handler (app db req-chan ws-map)))
