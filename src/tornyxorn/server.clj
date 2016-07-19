(ns tornyxorn.server
  (:require [com.stuartsierra.component :as component]
            [immutant.web :refer [run stop]]
            [clojure.tools.logging :as log]))

(defrecord WebServer [port server handler]
  component/Lifecycle
  (start [component]
    (if-not server
      (do
        (log/info "Starting web server on port" port)
        (assoc component :server (run (:handler handler) {:port port})))
      component))
  (stop [component]
    (when server
      (log/info "Shutting down web server")
      (stop server))
    (assoc component :server nil)))

(defn new-web-server [{:keys [port handler]}]
  (map->WebServer {:port port :handler handler}))
