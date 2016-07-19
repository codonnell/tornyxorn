(ns tornyxorn.response-types
  (:require [clojure.string :as string]
            [com.rpl.specter :as sp]
            [com.rpl.specter.macros :refer [select transform]]
            [clj-time.core :as t]
            [clj-time.format :as f]
            [clj-time.coerce :as c]
            [clojure.spec :as s]))

(defprotocol ResponseType
  (identify [self m]
    "Returns true if the response matches this type of response.")
  (extract [self m]
    "Extracts its own keys from a possibly larger api response.")
  (coerce [self m]
    "Coerces the response items into the appropriate types."))

(defn coerce-response [m items]
  (reduce (fn [amap item]
            (update amap (:spec-key item) (:coercer item)))
          m
          items))

(defrecord RespItem [torn-key spec-key coercer default])

(defrecord BasicInfoResponse [name items]
  ResponseType
  (identify [_ m]
    (and (= 5 (count m))
         (every? #(not= ::not-found (get-in m % ::not-found)) (map :torn-key items))))
  (extract [_ m]
    (reduce #(assoc %1 (:spec-key %2) (get-in m (:torn-key %2) (:default %2)))
            {} items))
  (coerce [_ m]
    (let [coerced-resp (coerce-response m items)]
      (if (s/valid? name coerced-resp)
        coerced-resp
        (throw (ex-info "Invalid API Response" (s/explain-data name coerced-resp)))))))

(defrecord BaseLevelResponse [name items]
  ResponseType
  (identify [_ m]
    (every?
     #(not= (get-in m % ::not-found) ::not-found)
     (select [(sp/filterer #(= ::required (:default %))) sp/ALL :torn-key] items)))
  (extract [_ m]
    (reduce #(assoc %1 (:spec-key %2) (get-in m (:torn-key %2) (:default %2)))
            {} items))
  (coerce [_ m]
    (let [coerced-resp (coerce-response m items)]
      (if (s/valid? name coerced-resp)
        coerced-resp
        (throw (ex-info "Invalid API Response" (s/explain-data name coerced-resp)))))))

(defrecord KeyedResponse [name key items]
  ResponseType
  (identify [self m]
    (contains? m key))
  (extract [self m]
    (reduce #(assoc %1 (:spec-key %2) (get-in m (:torn-key %2) (:default %2)))
            {} items))
  (coerce [self m]
    (let [coerced-resp (coerce-response m items)]
      (if (s/valid? name coerced-resp)
        coerced-resp
        (throw (ex-info "Invalid API Response" (s/explain-data name coerced-resp)))))))

(defn parse-last-action [s]
  (let [[n units & _] (string/split s #"\s")
        unit-fn (get {"minutes" t/minutes
                      "minute" t/minutes
                      "hours" t/hours
                      "hour" t/hours
                      "days" t/days
                      "day" t/days}
                     units)]
    (t/minus (t/now) (unit-fn (Integer/parseInt n)))))

;; Coercers

(defn empty->0 [n] (if (string? n) 0 n))
(defn nil->0 [n] (or n 0))
(defn nil->empty [s] (or s ""))
(defn timestamp->inst [n] (-> n (* 1000) (c/from-long)))
(defn int->pct [n] (double (inc (/ n 100))))
(def result-str->keyword
  {"Hospitalize" :attack.result/hospitalize
   "Stalemate" :attack.result/stalemate
   "Leave" :attack.result/leave
   "Mug" :attack.result/mug
   "Lose" :attack.result/lose
   "Run away" :attack.result/run-away
   "Timeout" :attack.result/timeout})

;; Response types

(def basic-info
  (->BasicInfoResponse
   :resp/basic-info
   [(->RespItem [:level] :player/level identity ::required)
    (->RespItem [:player_id] :player/torn-id identity ::required)
    (->RespItem [:name] :player/name identity ::required)]))

(def player-info
  (->BaseLevelResponse
   :resp/player-info
   [(->RespItem [:name] :player/name identity ::required)
    (->RespItem [:player_id] :player/torn-id identity ::required)
    (->RespItem [:signup] :player/signup #(f/parse (f/formatters :mysql) %) ::required)
    (->RespItem [:last_action] :player/last-action parse-last-action ::required)
    (->RespItem [:level] :player/level identity ::required)
    (->RespItem [:awards] :player/awards identity ::required)
    (->RespItem [:life :maximum] :player/max-life identity ::required)
    (->RespItem [:personalstats :logins] :player/logins identity 0)
    (->RespItem [:personalstats :useractivity] :player/activity identity 0)
    (->RespItem [:personalstats :attackslost] :player/attacks-lost identity 0)
    (->RespItem [:personalstats :attackswon] :player/attacks-won identity 0)
    (->RespItem [:personalstats :attacksdraw] :player/attacks-draw identity 0)
    (->RespItem [:personalstats :highestbeaten] :player/highest-beaten identity 0)
    (->RespItem [:personalstats :bestkillstreak] :player/best-kill-streak identity 0)
    (->RespItem [:personalstats :defendslost] :player/defends-lost identity 0)
    (->RespItem [:personalstats :defendswon] :player/defends-won identity 0)
    (->RespItem [:personalstats :defendsstalemated] :player/defends-draw identity 0)
    (->RespItem [:personalstats :xantaken] :player/xanax-taken identity 0)
    (->RespItem [:personalstats :exttaken] :player/ecstasy-taken identity 0)
    (->RespItem [:personalstats :traveltimes] :player/times-traveled identity 0)
    (->RespItem [:personalstats :networth] :player/networth identity 0)
    (->RespItem [:personalstats :refills] :player/refills nil->0 0)
    (->RespItem [:personalstats :statenhancersused] :player/stat-enhancers-used identity 0)
    (->RespItem [:personalstats :medicalitemsused] :player/medical-items-used identity 0)]))

(def battle-stats
  (->BaseLevelResponse
   :resp/battle-stats
   [(->RespItem [:strength] :player/strength identity ::required)
    (->RespItem [:dexterity] :player/dexterity identity ::required)
    (->RespItem [:defense] :player/defense identity ::required)
    (->RespItem [:speed] :player/speed identity ::required)
    (->RespItem [:strength_modifier] :player/strength-modifier int->pct ::required)
    (->RespItem [:dexterity_modifier] :player/dexterity-modifier int->pct ::required)
    (->RespItem [:defense_modifier] :player/defense-modifier int->pct ::required)
    (->RespItem [:speed_modifier] :player/speed-modifier int->pct ::required)]))

(def resp-types
  [basic-info player-info battle-stats])
