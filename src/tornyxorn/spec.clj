(ns tornyxorn.spec
  (:require [clojure.spec :as s]
            [clj-time.coerce :refer [from-date to-date]]))

;; Player attributes

(def date-spec
  (s/spec-impl '(fn [d] (cond (inst? d) d
                              (instance? org.joda.time.DateTime d) (to-date d)
                              :else ::s/invalid))
               (fn [d] (cond (inst? d) d
                             (instance? org.joda.time.DateTime d) (to-date d)
                             :else ::s/invalid))
               #(s/gen inst?)
               true
               (fn [d] (cond (instance? org.joda.time.DateTime d) d
                             (inst? d) (from-date d)
                             :else ::s/invalid))))

(s/def :player/torn-id pos-int?)
(s/def :player/api-key string?)
(s/def :player/level nat-int?)
(s/def :player/name string?)
(s/def :player/signup date-spec)
(s/def :player/last-action date-spec)
(s/def :player/awards nat-int?)
(s/def :player/max-life pos-int?)
;; new
(s/def :player/property-id nat-int?)
(s/def :player/friends nat-int?)
(s/def :player/enemies nat-int?)
(s/def :player/forum-posts nat-int?)
(s/def :player/karma nat-int?)
(s/def :player/role #{:player.role/admin :player.role/secretary :player.role/moderator
                      :player.role/helper :player.role/npc :player.role/civilian})
(s/def :player/donator? boolean?)
(s/def :player/company-id (s/nilable pos-int?))
(s/def :player/position (s/nilable string?))
(s/def :player/spouse (s/nilable pos-int?))
(s/def :player/faction (s/nilable pos-int?))


(s/def :player/strength (s/and double? pos?))
(s/def :player/dexterity (s/and double? pos?))
(s/def :player/speed (s/and double? pos?))
(s/def :player/defense (s/and double? pos?))
(s/def :player/strength-modifier double?)
(s/def :player/dexterity-modifier double?)
(s/def :player/speed-modifier double?)
(s/def :player/defense-modifier double?)

(s/def :player/logins nat-int?)
(s/def :player/activity nat-int?)
(s/def :player/attacks-lost nat-int?)
(s/def :player/attacks-won nat-int?)
(s/def :player/attacks-draw nat-int?)
(s/def :player/highest-beaten nat-int?)
(s/def :player/best-kill-streak nat-int?)
(s/def :player/defends-lost nat-int?)
(s/def :player/defends-won nat-int?)
(s/def :player/defends-draw nat-int?)
(s/def :player/xanax-taken nat-int?)
(s/def :player/ecstasy-taken nat-int?)
(s/def :player/times-traveled nat-int?)
(s/def :player/networth int?)
(s/def :player/refills nat-int?)
(s/def :player/stat-enhancers-used nat-int?)
(s/def :player/medical-items-used nat-int?)
;; new
(s/def :player/weapons-bought nat-int?)
(s/def :player/times-jailed nat-int?)
(s/def :player/dump-searches nat-int?)
(s/def :player/dump-finds nat-int?)
(s/def :player/items-dumped nat-int?)
(s/def :player/points-sold nat-int?)
(s/def :player/points-bought nat-int?)
(s/def :player/days-as-donator nat-int?)
(s/def :player/rounds-fired nat-int?)
(s/def :player/times-hospitalized nat-int?)
(s/def :player/critical-hits nat-int?)
(s/def :player/shotgun-hits nat-int?)
(s/def :player/temp-hits nat-int?)
(s/def :player/clubbed-hits nat-int?)
(s/def :player/machine-gun-hits nat-int?)
(s/def :player/artillery-hits nat-int?)
(s/def :player/slashing-hits nat-int?)
(s/def :player/mechanical-hits nat-int?)
(s/def :player/rifle-hits nat-int?)
(s/def :player/smg-hits nat-int?)
(s/def :player/piercing-hits nat-int?)
(s/def :player/pistol-hits nat-int?)
(s/def :player/city-finds nat-int?)
(s/def :player/money-mugged nat-int?)
(s/def :player/largest-mug nat-int?)
(s/def :player/drugs-taken nat-int?)
(s/def :player/overdoses nat-int?)
(s/def :player/pcp-taken nat-int?)
(s/def :player/speed-taken nat-int?)
(s/def :player/canabis-taken nat-int?)
(s/def :player/vicodin-taken nat-int?)
(s/def :player/shrooms-taken nat-int?)
(s/def :player/ketamine-taken nat-int?)
(s/def :player/lsd-taken nat-int?)
(s/def :player/opium-taken nat-int?)
(s/def :player/attacks-stealthed nat-int?)
(s/def :player/attacks-assisted nat-int?)
(s/def :player/attacks-ran-away nat-int?)
(s/def :player/defends-ran-away nat-int?)
(s/def :player/mail-sent nat-int?)
(s/def :player/faction-mail-sent nat-int?)
(s/def :player/friend-mail-sent nat-int?)
(s/def :player/company-mail-sent nat-int?)
(s/def :player/spouse-mail-sent nat-int?)
(s/def :player/trains-received nat-int?)
(s/def :player/mexico-travel nat-int?)
(s/def :player/london-travel nat-int?)
(s/def :player/switzerland-travel nat-int?)
(s/def :player/canada-travel nat-int?)
(s/def :player/hawaii-travel nat-int?)
(s/def :player/caymans-travel nat-int?)
(s/def :player/japan-travel nat-int?)
(s/def :player/dubai-travel nat-int?)
(s/def :player/sa-travel nat-int?)
(s/def :player/china-travel nat-int?)
(s/def :player/argentina-travel nat-int?)
(s/def :player/items-bought-abroad nat-int?)
(s/def :player/items-bought nat-int?)
(s/def :player/items-sent nat-int?)
(s/def :player/bazaar-customers nat-int?)
(s/def :player/bazaar-sales nat-int?)
(s/def :player/bazaar-profit nat-int?)
(s/def :player/bounties-received nat-int?)
(s/def :player/bounties-placed nat-int?)
(s/def :player/total-bounty-spent nat-int?)
(s/def :player/bounties-collected nat-int?)
(s/def :player/total-bounty-rewards nat-int?)
(s/def :player/viruses-coded nat-int?)
(s/def :player/total-respect nat-int?)
(s/def :player/revives-received nat-int?)
(s/def :player/revives nat-int?)
(s/def :player/trades nat-int?)
(s/def :player/auctions-won nat-int?)
(s/def :player/auctions-sold nat-int?)
(s/def :player/blood-withdrawn nat-int?)
(s/def :player/people-busted nat-int?)
(s/def :player/failed-busts nat-int?)
(s/def :player/bails-bought nat-int?)
(s/def :player/bails-spent nat-int?)
(s/def :player/mission-credits nat-int?)
(s/def :player/contracts-completed nat-int?)
(s/def :player/duke-contracts-completed nat-int?)
(s/def :player/missions-completed nat-int?)
(s/def :player/merits-bought nat-int?)
(s/def :player/personals-placed nat-int?)
(s/def :player/classifieds-placed nat-int?)

;; Attack attributes

(def result-spec #{:attack.result/hospitalize :attack.result/stalemate
                   :attack.result/leave :attack.result/mug :attack.result/lose
                   :attack.result/run-away :attack.result/timeout})

(s/def :attack/torn-id pos-int?)
(s/def :attack/timestamp-started date-spec)
(s/def :attack/timestamp-ended date-spec)
(s/def :attack/attacker (s/nilable pos-int?))
(s/def :attack/defender pos-int?)
(s/def :attack/result result-spec)
(s/def :attack/respect (s/double-in :infinite? false :NaN? false :min 0))
(s/def :attack/attacker-faction (s/nilable pos-int?))
(s/def :attack/defender-faction (s/nilable pos-int?))

(s/def :attack/attack
  (s/keys :req [:attack/torn-id :attack/timestamp-started :attack/timestamp-ended
                :attack/attacker :attack/defender :attack/result :attack/respect]))

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
                :player/stat-enhancers-used :player/medical-items-used
                :player/property-id :player/friends :player/enemies
                :player/forum-posts :player/karma :player/donator?
                :player/company-id :player/position :player/spouse
                :player/faction :player/weapons-bought :player/times-jailed
                :player/dump-searches :player/dump-finds :player/items-dumped
                :player/points-sold :player/points-bought
                :player/days-as-donator :player/rounds-fired
                :player/times-hospitalized :player/critical-hits
                :player/shotgun-hits :player/temp-hits :player/clubbed-hits
                :player/machine-gun-hits :player/artillery-hits
                :player/slashing-hits :player/mechanical-hits :player/rifle-hits
                :player/smg-hits :player/piercing-hits :player/pistol-hits
                :player/city-finds :player/money-mugged :player/largest-mug
                :player/drugs-taken :player/overdoses :player/pcp-taken
                :player/speed-taken :player/canabis-taken :player/vicodin-taken
                :player/shrooms-taken :player/ketamine-taken :player/lsd-taken
                :player/opium-taken :player/attacks-stealthed
                :player/attacks-assisted :player/attacks-ran-away
                :player/defends-ran-away :player/mail-sent
                :player/faction-mail-sent :player/friend-mail-sent
                :player/company-mail-sent :player/spouse-mail-sent
                :player/trains-received :player/mexico-travel
                :player/london-travel :player/switzerland-travel
                :player/canada-travel :player/hawaii-travel
                :player/caymans-travel :player/japan-travel :player/dubai-travel
                :player/sa-travel :player/china-travel :player/argentina-travel
                :player/items-bought-abroad :player/items-bought
                :player/items-sent :player/bazaar-customers :player/bazaar-sales
                :player/bazaar-profit :player/bounties-received
                :player/bounties-placed :player/total-bounty-spent
                :player/bounties-collected :player/total-bounty-rewards
                :player/viruses-coded :player/total-respect
                :player/revives-received :player/revives :player/trades
                :player/auctions-won :player/auctions-sold
                :player/blood-withdrawn :player/people-busted
                :player/failed-busts :player/bails-bought :player/bails-spent
                :player/mission-credits :player/contracts-completed
                :player/duke-contracts-completed :player/missions-completed
                :player/merits-bought :player/personals-placed
                :player/classifieds-placed]))

(s/def :resp/basic-info
  (s/keys :req [:player/level :player/torn-id :player/name]))

(s/def :resp/battle-stats
  (s/keys :req [:player/strength :player/dexterity :player/speed :player/defense
                :player/strength-modifier :player/dexterity-modifier
                :player/speed-modifier :player/defense-modifier]))

(s/def :resp/attacks (s/coll-of :attack/attack))
