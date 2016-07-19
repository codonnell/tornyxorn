(ns tornyxorn.repl
  (:require [clojure.tools.nrepl.server :refer [start-server stop-server]]
            [com.stuartsierra.component :as component]
            [clojure.tools.logging :as log]))


(defrecord Repl [port server]
  component/Lifecycle

  (start [component]
    (log/info "Starting repl server on port " port ".")
    (assoc component :server (start-server :port port)))

  (stop [component]
    (when server
      (log/info "Shutting down repl server.")
      (stop-server server))
    (assoc component :server nil)))

(defn new-repl-server [port]
  (map->Repl {:port port}))
