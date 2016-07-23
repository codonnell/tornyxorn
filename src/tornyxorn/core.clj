(ns tornyxorn.core
  (:require [com.stuartsierra.component :as component]
            [tornyxorn.system :refer [prod-system]])
  (:gen-class))

(defn -main [& args]
  (component/start-system (prod-system)))
