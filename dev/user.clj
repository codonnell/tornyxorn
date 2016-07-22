(ns user
  (:require
   [reloaded.repl :refer [system init start stop go reset reset-all]]
   [clojure.repl :refer :all]
   [tornyxorn.system :refer [dev-system]]))

(reloaded.repl/set-init! dev-system)
