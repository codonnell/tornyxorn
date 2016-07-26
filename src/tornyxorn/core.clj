(ns tornyxorn.core
  (:require [com.stuartsierra.component :as component]
            [tornyxorn.system :refer [prod-system]])
  (:gen-class))

(defn -main [& args]
  (def system (prod-system))
  (alter-var-root #'system component/start-system))
