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
  (identify [_ m]
    (contains? m key))
  (extract [_ m]
    (reduce #(assoc %1 (:spec-key %2) (get-in m (:torn-key %2) (:default %2)))
            {} items))

  (coerce [_ m]
    (let [coerced-resp (coerce-response m items)]
      (if (s/valid? name coerced-resp)
        coerced-resp
        (throw (ex-info "Invalid API Response" (s/explain-data name coerced-resp)))))))

(defrecord KeyedListResponse [name key items]
  ResponseType
  (identify [_ m]
    (contains? m key))
  (extract [_ m]
    (transform [(sp/view #(-> % (get key) vec))
                (sp/transformed sp/ALL (fn [[k v]]
                                         (assoc v :torn-id (-> k clojure.core/name Long/parseLong))))
                sp/ALL]
               (fn [attack]
                 (reduce #(assoc %1 (:spec-key %2) (get-in attack (:torn-key %2) (:default %2)))
                         {} items))
               m))
  (coerce [_ m]
    (let [coerced-resp (mapv #(coerce-response % items) m)]
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

(defn empty->nil [n] (if (string? n) nil n))
(defn nil->0 [n] (or n 0))
(defn zero->nil [n] (if (zero? n) nil n))
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
(def role-str->keyword
  {"Admin" :player.role/admin
   "Secretary" :player.role/secretary
   "Moderator" :player.role/moderator
   "Helper" :player.role/helper
   "NPC" :player.role/npc
   "Civilian" :player.role/civilian
   "Reporter" :player.role/reporter})

;; Response types

(def basic-info
  (->BasicInfoResponse
   :resp/basic-info
   [(->RespItem [:level] :player/level identity ::required)
    (->RespItem [:player_id] :player/torn-id identity ::required)
    (->RespItem [:name] :player/name str ::required)]))

(def player-info
  (->BaseLevelResponse
   :resp/player-info
   [(->RespItem [:name] :player/name str ::required)
    (->RespItem [:player_id] :player/torn-id identity ::required)
    (->RespItem [:signup] :player/signup #(f/parse (f/formatters :mysql) %) ::required)
    (->RespItem [:last_action] :player/last-action parse-last-action ::required)
    (->RespItem [:level] :player/level identity ::required)
    (->RespItem [:awards] :player/awards identity ::required)
    (->RespItem [:life :maximum] :player/max-life identity ::required)
    (->RespItem [:property_id] :player/property-id identity ::required)
    (->RespItem [:friends] :player/friends identity ::required)
    (->RespItem [:enemies] :player/enemies identity ::required)
    (->RespItem [:karma] :player/karma identity ::required)
    (->RespItem [:forum_posts] :player/forum-posts nil->0 ::required)
    (->RespItem [:role] :player/role role-str->keyword ::required)
    (->RespItem [:donator] :player/donator? #(= 1 %) ::required)
    (->RespItem [:job :company_id] :player/company-id zero->nil ::required)
    (->RespItem [:job :position] :player/position #(if (= "None" %) nil %) ::required)
    (->RespItem [:married :spouse_id] :player/spouse zero->nil ::required)
    (->RespItem [:faction :faction_id] :player/faction zero->nil ::required)
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
    (->RespItem [:personalstats :medicalitemsused] :player/medical-items-used identity 0)
    (->RespItem [:personalstats :weaponsbought] :player/weapons-bought identity 0)
    (->RespItem [:personalstats :bazaarcustomers] :player/bazaar-customers identity 0)
    (->RespItem [:personalstats :bazaarsales] :player/bazaar-sales identity 0)
    (->RespItem [:personalstats :bazaarprofit] :player/bazaar-profit identity 0)
    (->RespItem [:personalstats :pointsbought] :player/points-bought identity 0)
    (->RespItem [:personalstats :pointssold] :player/points-sold identity 0)
    (->RespItem [:personalstats :itemsboughtabroad] :player/items-bought-abroad identity 0)
    (->RespItem [:personalstats :itemsbought] :player/items-bought identity 0)
    (->RespItem [:personalstats :trades] :player/trades identity 0)
    (->RespItem [:personalstats :itemssent] :player/items-sent identity 0)
    (->RespItem [:personalstats :auctionswon] :player/auctions-won identity 0)
    (->RespItem [:personalstats :auctionsells] :player/auctions-sold identity 0)
    (->RespItem [:personalstats :moneymugged] :player/money-mugged identity 0)
    (->RespItem [:personalstats :attacksstealthed] :player/attacks-stealthed identity 0)
    (->RespItem [:personalstats :attackscriticalhits] :player/critical-hits identity 0)
    (->RespItem [:personalstats :respectforfaction] :player/total-respect identity 0)
    (->RespItem [:personalstats :roundsfired] :player/rounds-fired identity 0)
    (->RespItem [:personalstats :yourunaway] :player/attacks-ran-away identity 0)
    (->RespItem [:personalstats :theyrunaway] :player/defends-ran-away identity 0)
    (->RespItem [:personalstats :peoplebusted] :player/people-busted identity 0)
    (->RespItem [:personalstats :failedbusts] :player/failed-busts identity 0)
    (->RespItem [:personalstats :peoplebought] :player/bails-bought identity 0)
    (->RespItem [:personalstats :peopleboughtspent] :player/bails-spent identity 0)
    (->RespItem [:personalstats :virusescoded] :player/viruses-coded identity 0)
    (->RespItem [:personalstats :cityfinds] :player/city-finds identity 0)
    (->RespItem [:personalstats :bountiesplaced] :player/bounties-placed identity 0)
    (->RespItem [:personalstats :bountiesreceived] :player/bounties-received identity 0)
    (->RespItem [:personalstats :bountiescollected] :player/bounties-collected identity 0)
    (->RespItem [:personalstats :totalbountyreward] :player/total-bounty-rewards identity 0)
    (->RespItem [:personalstats :totalbountyspent] :player/total-bounty-spent identity 0)
    (->RespItem [:personalstats :revives] :player/revives identity 0)
    (->RespItem [:personalstats :revivesreceived] :player/revives-received identity 0)
    (->RespItem [:personalstats :trainsreceived] :player/trains-received identity 0)
    (->RespItem [:personalstats :drugsused] :player/drugs-taken identity 0)
    (->RespItem [:personalstats :overdosed] :player/overdoses identity 0)
    (->RespItem [:personalstats :meritsbought] :player/merits-bought identity 0)
    (->RespItem [:personalstats :personalsplaced] :player/personals-placed identity 0)
    (->RespItem [:personalstats :classifiedadsplaced] :player/classifieds-placed identity 0)
    (->RespItem [:personalstats :mailssent] :player/mail-sent identity 0)
    (->RespItem [:personalstats :friendmailssent] :player/faction-mail-sent identity 0)
    (->RespItem [:personalstats :factionmailssent] :player/friend-mail-sent identity 0)
    (->RespItem [:personalstats :companymailssent] :player/company-mail-sent identity 0)
    (->RespItem [:personalstats :spousemailssent] :player/spouse-mail-sent identity 0)
    (->RespItem [:personalstats :largestmug] :player/largest-mug identity 0)
    (->RespItem [:personalstats :cantaken] :player/canabis-taken identity 0)
    (->RespItem [:personalstats :kettaken] :player/ketamine-taken identity 0)
    (->RespItem [:personalstats :lsdtaken] :player/lsd-taken identity 0)
    (->RespItem [:personalstats :opitaken] :player/opium-taken identity 0)
    (->RespItem [:personalstats :shrtaken] :player/shrooms-taken identity 0)
    (->RespItem [:personalstats :spetaken] :player/speed-taken identity 0)
    (->RespItem [:personalstats :pcptaken] :player/pcp-taken identity 0)
    (->RespItem [:personalstats :victaken] :player/vicodin-taken identity 0)
    (->RespItem [:personalstats :chahits] :player/mechanical-hits identity 0)
    (->RespItem [:personalstats :heahits] :player/artillery-hits identity 0)
    (->RespItem [:personalstats :axehits] :player/clubbed-hits identity 0)
    (->RespItem [:personalstats :grehits] :player/temp-hits identity 0)
    (->RespItem [:personalstats :machits] :player/machine-gun-hits identity 0)
    (->RespItem [:personalstats :pishits] :player/pistol-hits identity 0)
    (->RespItem [:personalstats :rifhits] :player/rifle-hits identity 0)
    (->RespItem [:personalstats :shohits] :player/shotgun-hits identity 0)
    (->RespItem [:personalstats :smghits] :player/smg-hits identity 0)
    (->RespItem [:personalstats :piehits] :player/piercing-hits identity 0)
    (->RespItem [:personalstats :slahits] :player/slashing-hits identity 0)
    (->RespItem [:personalstats :argtravel] :player/argentina-travel identity 0)
    (->RespItem [:personalstats :mextravel] :player/mexico-travel identity 0)
    (->RespItem [:personalstats :dubtravel] :player/dubai-travel identity 0)
    (->RespItem [:personalstats :hawtravel] :player/hawaii-travel identity 0)
    (->RespItem [:personalstats :japtravel] :player/japan-travel identity 0)
    (->RespItem [:personalstats :lontravel] :player/london-travel identity 0)
    (->RespItem [:personalstats :soutravel] :player/sa-travel identity 0)
    (->RespItem [:personalstats :switravel] :player/switzerland-travel identity 0)
    (->RespItem [:personalstats :chitravel] :player/china-travel identity 0)
    (->RespItem [:personalstats :cantravel] :player/canada-travel identity 0)
    (->RespItem [:personalstats :dumpfinds] :player/dump-finds identity 0)
    (->RespItem [:personalstats :dumpsearches] :player/dump-searches identity 0)
    (->RespItem [:personalstats :itemsdumped] :player/items-dumped identity 0)
    (->RespItem [:personalstats :daysbeendonator] :player/days-as-donator identity 0)
    (->RespItem [:personalstats :caytravel] :player/caymans-travel identity 0)
    (->RespItem [:personalstats :jailed] :player/times-jailed identity 0)
    (->RespItem [:personalstats :hospital] :player/times-hospitalized identity 0)
    (->RespItem [:personalstats :attacksassisted] :player/attacks-assisted identity 0)
    (->RespItem [:personalstats :bloodwithdrawn] :player/blood-withdrawn identity 0)
    (->RespItem [:personalstats :missioncreditsearned] :player/mission-credits identity 0)
    (->RespItem [:personalstats :contractscompleted] :player/contracts-completed identity 0)
    (->RespItem [:personalstats :dukecontractscompleted] :player/duke-contracts-completed identity 0)
    (->RespItem [:personalstats :missionscompleted] :player/missions-completed identity 0)
    ]))

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

(def attacks
  (->KeyedListResponse
   :resp/attacks
   :attacks
   [(->RespItem [:torn-id] :attack/torn-id identity ::required)
    (->RespItem [:timestamp_started] :attack/timestamp-started timestamp->inst ::required)
    (->RespItem [:timestamp_ended] :attack/timestamp-ended timestamp->inst ::required)
    (->RespItem [:attacker_id] :attack/attacker empty->nil ::required)
    (->RespItem [:attacker_faction] :attack/attacker-faction
                #(if (or (= "" %) (= 0 %)) nil %) ::required)
    (->RespItem [:defender_id] :attack/defender identity ::required)
    (->RespItem [:defender_faction] :attack/defender-faction zero->nil ::required)
    (->RespItem [:result] :attack/result result-str->keyword ::required)
    (->RespItem [:respect_gain] :attack/respect double ::required)]))

(def resp-types
  [basic-info player-info battle-stats attacks])
