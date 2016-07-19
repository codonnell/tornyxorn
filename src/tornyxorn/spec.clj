(ns tornyxorn.spec
  (:require [clojure.spec :as s]
            [clojure.spec.gen :as gen]
            [clj-time.coerce :refer [from-date to-date]]))

;; Player attributes

(def nonneg-int?
  (s/with-gen
    (s/conformer (fn [x] (if (and (integer? x) (>= x 0))
                           x
                           ::s/invalid))
                 identity)
    (fn [] (gen/such-that #(>= % 0) (s/gen integer?)))))

(def date-spec
  (s/spec-impl '(fn [d] (cond (inst? d) d
                              (instance? org.joda.time.DateTime d) (to-date d)
                              :else ::s/invalid))
               (fn [d] (cond (inst? d) d
                             (instance? org.joda.time.DateTime d) (to-date d)
                             :else ::s/invalid))
               true
               (fn [d] (cond (instance? org.joda.time.DateTime d) d
                             (inst? d) (from-date d)
                             :else ::s/invalid))))

(s/def :player/torn-id pos-int?)
(s/def :player/api-key string?)
(s/def :player/level pos-int?)
(s/def :player/name string?)
(s/def :player/signup date-spec)
(s/def :player/last-action date-spec)
(s/def :player/awards nonneg-int?)
(s/def :player/max-life pos-int?)

(s/def :player/strength (s/and double? pos?))
(s/def :player/dexterity (s/and double? pos?))
(s/def :player/speed (s/and double? pos?))
(s/def :player/defense (s/and double? pos?))
(s/def :player/strength-modifier double?)
(s/def :player/dexterity-modifier double?)
(s/def :player/speed-modifier double?)
(s/def :player/defense-modifier double?)

(s/def :player/logins nonneg-int?)
(s/def :player/activity nonneg-int?)
(s/def :player/attacks-lost nonneg-int?)
(s/def :player/attacks-won nonneg-int?)
(s/def :player/attacks-draw nonneg-int?)
(s/def :player/highest-beaten nonneg-int?)
(s/def :player/best-kill-streak nonneg-int?)
(s/def :player/defends-lost nonneg-int?)
(s/def :player/defends-won nonneg-int?)
(s/def :player/defends-draw nonneg-int?)
(s/def :player/xanax-taken nonneg-int?)
(s/def :player/ecstasy-taken nonneg-int?)
(s/def :player/times-traveled nonneg-int?)
(s/def :player/networth nonneg-int?)
(s/def :player/refills nonneg-int?)
(s/def :player/stat-enhancers-used nonneg-int?)
(s/def :player/medical-items-used nonneg-int?)

;; Message types

(s/def :ws-msg/type #{"players" "submit-api-key"})

(s/def :msg/type #{:msg/players :msg/submit-api-key})

(s/def :msg/ids (s/coll-of :player/torn-id))
(s/def :msg/ws any?)

;; Websocket messages

(defmulti ws-msg-type :type)
(defmethod ws-msg-type "players" [_]
  (s/keys :req-un [:ws-msg/type :msg/ids :player/api-key :msg/ws]))
(defmethod ws-msg-type "submit-api-key" [_]
  (s/keys :req-un [:ws-msg/type :player/api-key :msg/ws]))
(s/def :ws-msg/msg (s/multi-spec ws-msg-type :type))


;; Intra-app messages

(defmulti msg-type :msg/type)
(defmethod msg-type :msg/players [_]
  (s/keys :req [:msg/type :msg/ws :msg/ids :player/api-key]))
(defmethod msg-type :msg/submit-api-key [_]
  (s/keys :req [:msg/type :msg/ws :player/api-key]))
(s/def :msg/msg (s/multi-spec msg-type :msg/type))

;; Torn API Responses

(s/def :resp/player-info
  (s/keys :req [:player/torn-id :player/level :player/name :player/signup
                :player/last-action :player/awards :player/max-life
                :player/logins :player/activity :player/attacks-lost
                :player/attacks-won :player/attacks-draw :player/highest-beaten
                :player/best-kill-streak :player/defends-lost :player/defends-won
                :player/defends-draw :player/xanax-taken :player/ecstasy-taken
                :player/times-traveled :player/networth :player/refills
                :player/stat-enhancers-used :player/medical-items-used]))

(s/def :resp/basic-info
  (s/keys :req [:player/level :player/torn-id :player/name]))

(s/def :resp/battle-stats
  (s/keys :req [:player/strength :player/dexterity :player/speed :player/defense
                :player/strength-modifier :player/dexterity-modifier
                :player/speed-modifier :player/defense-modifier]))
