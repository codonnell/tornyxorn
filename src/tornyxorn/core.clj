(ns tornyxorn.core
  (:require [com.stuartsierra.component :as component]
            [tornyxorn.system :refer [dev-system]])
  (:gen-class))

(defn -main [& args]
  (component/start-system (dev-system)))
