(ns tornyxorn.log
  (:require [clojure.tools.logging :as log]
            [clojure.string :as string]
            [clojure.core.async :as a]))

(def log-c (a/chan 100))

(def sink (a/map (fn [{:keys [type msg]}]
                   (case type
                     :info (log/info msg)
                     :debug (log/debug msg)
                     :warn (log/warn msg)
                     :error (log/error msg)
                     :fatal (log/fatal msg))
                   :dummy) ;; so nil doesn't close our sink channel
                 [log-c]
                 (a/dropping-buffer 1)))

(defn info [& xs]
  (a/>!! log-c {:type :info :msg (string/join " " xs)}))

(defn debug [& xs]
  (a/>!! log-c {:type :debug :msg (string/join " " xs)}))

(defn warn [& xs]
  (a/>!! log-c {:type :warn :msg (string/join " " xs)}))

(defn error [& xs]
  (a/>!! log-c {:type :error :msg (string/join " " xs)}))

(defn fatal [& xs]
  (a/>!! log-c {:type :fatal :msg (string/join " " xs)}))
