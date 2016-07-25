(ns tornyxorn.util
  (:require [clojure.core.async :refer [go-loop alts! timeout >!]]))


(defn do-every [ms finish-c f]
  "Calls f every ms milliseconds. Finishes when finish-c is closed or receives a
  message."
  (go-loop []
    (let [[_ c] (alts! [(timeout ms) finish-c])]
      (when-not (= c finish-c)
        (f)
        (recur)))))
