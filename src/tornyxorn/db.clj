(ns tornyxorn.db
  (:require [datomic.api :as d]
            [clojure.spec :as s]
            [clojure.set :as set]
            [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [org.httpkit.client :as http]
            [cheshire.core :as json]
            [clj-time.core :as t]
            [clj-time.coerce :refer [to-date from-date to-long]]
            [com.rpl.specter :as sp]
            [com.rpl.specter.macros :refer [select transform]]
            [com.stuartsierra.component :as component]
            [clojure.tools.logging :as log]))

(defrecord Datomic [uri conn]
  component/Lifecycle

  (start [component]
    (d/create-database uri)
    (let [conn (d/connect uri)]
      @(d/transact conn (read-string (slurp "resources/schema.edn")))
      (assoc component :conn conn)))

  (stop [component]
    (when conn (d/release conn))
    (assoc component :conn nil)))

(defn new-datomic-db [uri]
  (map->Datomic {:uri uri}))




;; Database functions.
;;
;; Functions with a * suffix take a database value or connection as an argument.
;; Functions without the * suffix take a database component (a la Stuart Sierra)
;; as an argument.



(defn player-by-id* [db torn-id]
  (d/entity db (d/q '[:find ?p . :in $ ?id :where [?p :player/torn-id ?id]] db torn-id)))

(defn player-by-id [db torn-id]
  (player-by-id* (-> db :conn d/db) torn-id))


(defn player-by-api-key* [db api-key]
  (d/entity db (d/q '[:find ?p . :in $ ?k :where [?p :player/api-key ?k]] db api-key)))

(defn player-by-api-key [db api-key]
  (player-by-api-key* (-> db :conn d/db) api-key))


(defn add-tempid [data]
  (assoc data :db/id (d/tempid :db.part/user)))


(defn has-player-info?* [db torn-id]
  (not (nil? (:player/last-info-update (player-by-id* db torn-id)))))

(defn has-player-info? [db torn-id]
  (has-player-info?* (-> db :conn d/db) torn-id))



(defn api-keys* [db]
  (d/q '[:find [?k ...] :where (or [_ :player/api-key ?k] [_ :player/temp-api-key ?k])] db))

(defn api-keys [db]
  (api-keys* (-> db :conn d/db)))


(defn stale-players* [db n]
  "Returns a sequence of (up to) n players with the oldest player info that is
  at least a month old."
  (let [ps (->> (d/q '[:find [?p ...]
                       :where [?p :player/torn-id]
                       (not [?p :player/last-player-info-update])]
                     db)
                (take n)
                (map (partial d/entity db)))
        k (count ps)]
    (if (= n k)
      ps
      (->> (d/q '[:find [?p ...]
                  :where [?p :player/last-player-info-update]]
                db)
           (map (partial d/entity db))
           (sort-by :player/last-player-info-update)
           (take (- n k))
           (take-while #(t/before? (from-date (:player/last-player-info-update %))
                                   (t/ago (t/months 1))))
           (concat ps)))))

(defn stale-players [db n]
  (stale-players* (-> db :conn d/db) n))


(defn add-player-tx [player]
  [(add-tempid player)])

(defn add-player* [conn player]
  (d/transact conn (add-player-tx player)))

(defn add-player [db player]
  (add-player* (:conn db) player))


(defn remove-nils [m]
  (into {} (remove (fn [[k v]] (nil? v))) m))

(defn schema-player-info->db-player-info [{:keys [player/faction] :as info}]
  (let [parsed-info (s/conform :resp/player-info info)]
    (if-not (= ::s/invalid parsed-info)
      (as-> parsed-info info
        (remove-nils info)
        (update info :player/role (fn [role] [:db/ident role]))
        (assoc info
               :player/last-player-info-update (to-date (t/now)))
        (if-not (nil? faction)
          (assoc info :player/faction {:faction/torn-id faction})
          (dissoc info :player/faction)))
      (throw (ex-info "Invalid player info" (s/explain-data :resp/player-info info))))))

(defn add-player-info-tx [info]
  [(-> info schema-player-info->db-player-info add-tempid)])

(defn add-player-info* [conn info]
  (d/transact conn (add-player-info-tx info)))

(defn add-player-info [db info]
  (add-player-info* (:conn db) info))


(defn update-battle-stats-tx [torn-id stats]
  (let [parsed-stats (s/conform :resp/battle-stats stats)]
    (if-not (= ::s/invalid parsed-stats)
      [(assoc parsed-stats
              :player/last-battle-stats-update (to-date (t/now))
              :player/torn-id torn-id
              :db/id (d/tempid :db.part/user))]
      (throw (ex-info "Invalid battle stats" (s/explain-data :resp/battle-stats stats))))))

(defn update-battle-stats* [conn torn-id stats]
  (d/transact conn (update-battle-stats-tx torn-id stats)))

(defn update-battle-stats [db torn-id stats]
  (update-battle-stats* (:conn db) torn-id stats))


(defn- remove-nil-attacker [attack]
  (if (nil? (:attack/attacker attack))
    (dissoc attack :attack/attacker)
    attack))

(defn schema-attack->db-attack
  [{:keys [attack/attacker attack/attacker-faction attack/defender-faction] :as attack}]
  (as-> attack a
    (assoc a
           :attack/defender {:player/torn-id (:attack/defender attack)}
           :attack/result [:db/ident (:attack/result attack)])
    (if-not (nil? attacker)
      (assoc a :attack/attacker {:player/torn-id attacker})
      (dissoc a :attack/attacker))
    (if-not (nil? attacker-faction)
      (assoc a :attack/attacker-faction {:faction/torn-id attacker-faction})
      (dissoc a :attack/attacker-faction))
    (if-not (nil? defender-faction)
      (assoc a :attack/defender-faction {:faction/torn-id defender-faction})
      (dissoc a :attack/defender-faction))
    (update a :attack/timestamp-started to-date)
    (update a :attack/timestamp-ended to-date)))

(defn total-stats [p]
  (+ (* (:player/strength p) (:player/strength-modifier p))
     (* (:player/dexterity p) (:player/dexterity-modifier p))
     (* (:player/speed p) (:player/speed-modifier p))
     (* (:player/defense p) (:player/defense-modifier p))))

(defn attack-success? [attack]
  (contains? #{:attack.result/hospitalize :attack.result/mug :attack.result/leave}
             (:attack/result attack)))

(defn attack-fail? [attack]
  (not (attack-success? attack)))

(defn difficulty-update*
  "Returns a map with :player/torn-id and (:player/lowest-win or
  :player/highest-lost) when they need updating. If neither lowest-win nor
  highest-loss need to be updated, returns nil."
  [db attack]
  (let [attacker (player-by-id* db (:attack/attacker attack))
        defender (player-by-id* db (:attack/defender attack))]
    (if (nil? (:player/strength attacker)) nil
        (if (attack-success? attack)
          (if (< (total-stats attacker) (or (:player/lowest-win defender) Double/MAX_VALUE))
            {:player/torn-id (:player/torn-id defender) :player/lowest-win (total-stats attacker)}
            nil)
          (if (> (total-stats attacker) (or (:player/highest-loss defender) 0.0))
            {:player/torn-id (:player/torn-id defender) :player/highest-loss (total-stats attacker)}
            nil)))))

(defn add-attacks-tx [db attacks]
  (mapv (comp add-tempid schema-attack->db-attack) attacks))

;; TODO: Add difficulty-update* in here somewhere
(defn add-attacks* [conn attacks]
  (d/transact conn (add-attacks-tx (d/db conn) attacks)))

(defn add-attacks [db attacks]
  (add-attacks* (:conn db) attacks))


(defn new-attacks* [db attacks]
  (let [new-ids (mapv :attack/torn-id attacks)
        curr-ids (d/q '[:find [?id ...]
                        :in $ [?id ...]
                        :where [_ :attack/torn-id ?id]]
                      db new-ids)]
    (set/difference (set new-ids) (set curr-ids))))

(defn new-attacks [db attacks]
  (new-attacks* (-> db :conn d/db) attacks))


(defn has-api-key?* [db torn-id]
  (d/q '[:find ?id .
         :in $ ?id
         :where [?p :player/torn-id ?id] [?p :player/api-key]]
       db torn-id))

(defn has-api-key? [db torn-id]
  (has-api-key?* (-> db :conn d/db) torn-id))


(defn add-basic-info-tx [info]
  (let [parsed-info (s/conform :resp/basic-info info)]
    (if-not (= ::s/invalid parsed-info)
      [(add-tempid parsed-info)]
      (throw (ex-info "Invalid basic info" (s/explain-data :resp/basic-info info))))))

(defn add-basic-info* [conn info]
  (d/transact conn (add-basic-info-tx info)))

(defn add-basic-info [db info]
  (add-basic-info* (:conn db) info))


(defn get-api-key* [db torn-id]
  (:player/api-key (player-by-id* db torn-id)))

(defn get-api-key [db torn-id]
  (get-api-key* (-> db :conn d/db) torn-id))

(defn add-api-key-tx [api-key]
  [{:db/id (d/tempid :db.part/user)
    :player/temp-api-key api-key}])

(defn add-api-key* [conn api-key]
  (d/transact conn (add-api-key-tx api-key)))

(defn add-api-key [db api-key]
  (add-api-key* (:conn db) api-key))


(defn remove-api-key* [conn api-key]
  (let [player-entid (->> api-key (player-by-api-key* (d/db conn)) :db/id)
        temp-entids (d/q '[:find [?p ...] :in $ ?k :where [?p :player/temp-api-key ?k]] (d/db conn) api-key)]
    (d/transact conn (filterv (fn [[_ entid _ _]] entid)
                              (conj (mapv (fn [entid]
                                            [:db/retract entid :player/temp-api-key api-key])
                                          temp-entids)
                                    [:db/retract player-entid :player/api-key api-key])))))

(defn remove-api-key [db api-key]
  (remove-api-key* (:conn db) api-key))

;; Difficulty computation functions

(defn easy-attack? [attacker attack]
  (and (attack-success? attack)
       (<= (total-stats (:attack/attacker attack))
           (total-stats attacker))))

(defn hard-attack? [attacker attack]
  (and (attack-fail? attack)
       (>= (total-stats (:attack/attacker attack))
           (total-stats attacker))))

(defn estimate-stats* [db id]
  (let [attacks (d/q '[:find [?a ...]
                       :in $ ?id
                       :where [?d :player/torn-id ?id]
                       [?a :attack/defender ?d]
                       [?a :attack/attacker ?ap]
                       [?ap :player/strength _]]
                     db id)
        groups (group-by attack-success? attacks)
        lowest-win (if (empty? (groups true)) nil
                       (apply min (map #(total-stats (:attack/attacker %)) (groups true))))
        highest-loss (if (empty? (groups false)) nil
                         (apply max (map #(total-stats (:attack/attacker %)) (groups false))))]
    (remove-nils {:player/lowest-win lowest-win :player/highest-loss highest-loss})))

(defn renew-difficulties* [conn]
  (let [db (d/db conn)
        ids (d/q '[:find [?id ...] :where [_ :player/torn-id ?id]] db)]
    (doseq [id ids]
      (d/transact conn [(add-tempid (assoc (estimate-stats* db id) :player/torn-id id))]))))

(defn renew-difficulties [db]
  (renew-difficulties* (:conn db)))

(defn difficulty* [db attacker defender]
  (let [a (total-stats attacker)
        l (or (:player/lowest-win defender) Double/MAX_VALUE)
        h (or (:player/highest-loss defender) 0.0)]
    (cond (and (> a l) (< a h)) [:medium 1.0]
          (> a l) [:easy 1.0]
          (< a h) [:impossible 1.0]
          :else [:unknown 1.0])))

#_(defn difficulty* [db attacker defender]
    "Returns :easy if someone with fewer total stats than attacker beat defender,
  :hard if someone with more total stats than attacker lost to defender, :medium
  of both of these are true, and :unknown if neither are true."
    (if-not (and (:player/strength attacker) (:player/torn-id defender))
      :unknown
      (let [attacks-on-d (map (fn [[a tx]] (d/entity #_(d/as-of db tx) db a))
                              (d/q '[:find ?a ?tx1
                                     :in $ ?id
                                     :where
                                     [?a :attack/defender ?d ?tx1]
                                     [?d :player/torn-id ?id]
                                     [?a :attack/attacker ?ap]
                                     [?ap :player/strength _ ?tx2]
                                     [(< ?tx2 ?tx1)]]
                                   db (:player/torn-id defender)))
            easy-attacks (filter (partial easy-attack? attacker) attacks-on-d)
            hard-attacks (filter (partial hard-attack? attacker) attacks-on-d)]
        (cond
          (and (empty? easy-attacks) (empty? hard-attacks)) [:unknown 1.0]
          (empty? hard-attacks) [:easy 1.0]
          (empty? easy-attacks) [:impossible 1.0]
          :else [:medium 1.0]))))

(defn difficulty [db attacker defender]
  (difficulty* (-> db :conn d/db) attacker defender))

(defn difficulties* [db attacker defenders]
  (mapv (partial difficulty* db attacker) defenders))

(defn difficulties [db attacker defenders]
  (difficulties* (-> db :conn d/db) attacker defenders))

;; Dump attack info to csv for learning

(defn change-ns [m ns]
  (transform [sp/ALL sp/FIRST]
             (fn [k] (keyword ns (name k)))
             m))

(def success-map
  {:attack.result/hospitalize :attack/win
   :attack.result/mug :attack/win
   :attack.result/leave :attack/win
   :attack.result/lose :attack/lose
   :attack.result/run-away :attack/lose
   :attack.result/stalemate :attack/lose
   :attack.result/timeout :attack/lose})

(def to-remove [:player/faction :player/spouse :player/position :player/company-id
                :player/property-id :player/name :player/api-key :player/last-player-info-update
                :player/role :player/last-battle-stats-update :player/strength-modifier
                :player/speed-modifier :player/dexterity-modifier :player/defense-modifier
                :player/donator?])

(def label-keys [:player/torn-id])

(defn with-mod-battle-stats [{:keys [player/strength player/strength-modifier
                                     player/dexterity player/dexterity-modifier
                                     player/speed player/speed-modifier
                                     player/defense player/defense-modifier] :as m}]
  (if strength
    (assoc m
           :player/strength (* strength (or strength-modifier 1.0))
           :player/dexterity (* dexterity (or dexterity-modifier 1.0))
           :player/speed (* speed (or speed-modifier 1.0))
           :player/defense (* defense (or defense-modifier 1.0)))
    m))

(defn transform-player [m role]
  (as-> (into {} m) m
    (with-mod-battle-stats m)
    (apply dissoc m to-remove)
    (update m :player/signup (comp to-long from-date))
    (update m :player/last-action (comp to-long from-date))
    (change-ns m role)))

(defn trainsform-player [m role]
  (->
   (apply dissoc m to-remove)
   (update :player/signup (comp to-long from-date))
   (update :player/last-action (comp to-long from-date))
   (change-ns m role)))

(defn pct-wins [as]
  (if (zero? (count as))
    nil
    (double (/ (count (filter #(identical? :attack/win (-> % :attack/result success-map)) as)) (count as)))))

(defn attack-pairs* [db]
  (d/q '[:find (pull ?at [:player/torn-id]) (pull ?d [:player/torn-id])
         :where [?at :player/strength]
         [?at :player/last-player-info-update]
         [?a :attack/attacker ?at]
         [?a :attack/defender ?d]
         [?d :player/last-player-info-update]]
       db))

(defn attack-pairs [db]
  (attack-pairs* (-> db :conn d/db)))

(defn attack-pair-data*
  "Returns a map containing attack data where at attacked d. Assumes at has
  player info and battle stats stored and d has player info stored."
  [db [at-id d-id]]
  (let [attacks (map #(d/entity db %)
                     (d/q '[:find [?a ...]
                            :in $ ?at-id ?d-id
                            :where [?at :player/torn-id ?at-id]
                            [?d :player/torn-id ?d-id]
                            [?a :attack/attacker ?at]
                            [?a :attack/defender ?d]]
                          db at-id d-id))
        attacker (transform-player (player-by-id* db at-id) "attacker")
        defender (transform-player (player-by-id* db d-id) "defender")
        win-chance (pct-wins attacks)]
    (merge attacker defender {:attack/win-chance win-chance})))

(defn attack-pair-data [db pair]
  (attack-pair-data* (-> db :conn d/db) pair))


(defn train-data* [db [at-id d-id]]
  (let [attacks (map #(d/entity db %)
                     (d/q '[:find [?a ...]
                            :in $ ?at-id ?d-id
                            :where [?at :player/torn-id ?at-id]
                            [?d :player/torn-id ?d-id]
                            [?a :attack/attacker ?at]
                            [?a :attack/defender ?d]]
                          db at-id d-id))
        attacker (transform-player (player-by-id* db at-id) "attacker")
        defender (transform-player (player-by-id* db d-id) "defender")
        win-chance (pct-wins attacks)]
    (merge attacker defender {:attack/win-chance win-chance :attack/count (count attacks)})))



(defn attack-data* [db]
  (let [tuples
        (d/q '[:find ?at ?d ?a
               :where [?at :player/strength]
               [?at :player/last-player-info-update]
               [?a :attack/attacker ?at]
               [?a :attack/defender ?d]
               [?d :player/last-player-info-update]]
             db)]
    (->> tuples
         (mapv (comp #(apply merge %)
                     (fn [[at d a]]
                       [(transform-player at "attacker")
                        (transform-player d "defender")
                        {:attack/result (-> a :attack/result success-map)}])
                     (fn [ents] (mapv #(d/entity db %) ents))))
         (group-by #([(:attacker/torn-id %) (:defender/torn-id %)]))
         vals
         (mapv (fn [as]
                 (-> (first as)
                     (assoc :attack/win-chance (pct-wins as))
                     (dissoc :attack/result)))))))

(defn attack-data [db]
  (attack-data* (-> db :conn d/db)))

(def data-indices
  {:attack/win-chance 0, :attacker/activity 1, :attacker/argentina-travel 2, :attacker/artillery-hits 3,
   :attacker/attacks-assisted 4, :attacker/attacks-draw 5, :attacker/attacks-lost 6,
   :attacker/attacks-ran-away 7, :attacker/attacks-stealthed 8, :attacker/attacks-won 9,
   :attacker/auctions-sold 10, :attacker/auctions-won 11, :attacker/awards 12, :attacker/bails-bought 13,
   :attacker/bails-spent 14, :attacker/bazaar-customers 15, :attacker/bazaar-profit 16,
   :attacker/bazaar-sales 17, :attacker/best-kill-streak 18, :attacker/blood-withdrawn 19,
   :attacker/bounties-collected 20, :attacker/bounties-placed 21, :attacker/bounties-received 22,
   :attacker/canabis-taken 23, :attacker/canada-travel 24, :attacker/caymans-travel 25,
   :attacker/china-travel 26, :attacker/city-finds 27, :attacker/classifieds-placed 28,
   :attacker/clubbed-hits 29, :attacker/company-mail-sent 30, :attacker/contracts-completed 31,
   :attacker/critical-hits 32, :attacker/days-as-donator 33, :attacker/defends-draw 34,
   :attacker/defends-lost 35, :attacker/defends-ran-away 36, :attacker/defends-won 37,
   :attacker/defense 38, :attacker/dexterity 39, :attacker/drugs-taken 40, :attacker/dubai-travel 41,
   :attacker/duke-contracts-completed 42, :attacker/dump-finds 43, :attacker/dump-searches 44,
   :attacker/ecstasy-taken 45, :attacker/enemies 46, :attacker/faction-mail-sent 47,
   :attacker/failed-busts 48, :attacker/forum-posts 49, :attacker/friend-mail-sent 50,
   :attacker/friends 51, :attacker/hawaii-travel 52, :attacker/highest-beaten 53, :attacker/items-bought 54,
   :attacker/items-bought-abroad 55, :attacker/items-dumped 56, :attacker/items-sent 57,
   :attacker/japan-travel 58, :attacker/karma 59, :attacker/ketamine-taken 60, :attacker/largest-mug 61,
   :attacker/last-action 62, :attacker/level 63, :attacker/logins 64, :attacker/london-travel 65,
   :attacker/lsd-taken 66, :attacker/machine-gun-hits 67, :attacker/mail-sent 68, :attacker/max-life 69,
   :attacker/mechanical-hits 70, :attacker/medical-items-used 71, :attacker/merits-bought 72,
   :attacker/mexico-travel 73, :attacker/mission-credits 74, :attacker/missions-completed 75,
   :attacker/money-mugged 76, :attacker/networth 77, :attacker/opium-taken 78, :attacker/overdoses 79,
   :attacker/pcp-taken 80, :attacker/people-busted 81, :attacker/personals-placed 82,
   :attacker/piercing-hits 83, :attacker/pistol-hits 84, :attacker/points-bought 85,
   :attacker/points-sold 86, :attacker/refills 87, :attacker/revives 88, :attacker/revives-received 89,
   :attacker/rifle-hits 90, :attacker/rounds-fired 91, :attacker/sa-travel 92, :attacker/shotgun-hits 93,
   :attacker/shrooms-taken 94, :attacker/signup 95, :attacker/slashing-hits 96, :attacker/smg-hits 97,
   :attacker/speed 98, :attacker/speed-taken 99, :attacker/spouse-mail-sent 100,
   :attacker/stat-enhancers-used 101, :attacker/strength 102, :attacker/switzerland-travel 103,
   :attacker/temp-hits 104, :attacker/times-hospitalized 105, :attacker/times-jailed 106,
   :attacker/times-traveled 107, :attacker/torn-id 108, :attacker/total-bounty-rewards 109,
   :attacker/total-bounty-spent 110, :attacker/total-respect 111, :attacker/trades 112,
   :attacker/trains-received 113, :attacker/vicodin-taken 114, :attacker/viruses-coded 115,
   :attacker/weapons-bought 116, :attacker/xanax-taken 117, :defender/activity 118,
   :defender/argentina-travel 119, :defender/artillery-hits 120, :defender/attacks-assisted 121,
   :defender/attacks-draw 122, :defender/attacks-lost 123, :defender/attacks-ran-away 124,
   :defender/attacks-stealthed 125, :defender/attacks-won 126, :defender/auctions-sold 127,
   :defender/auctions-won 128, :defender/awards 129, :defender/bails-bought 130, :defender/bails-spent 131,
   :defender/bazaar-customers 132, :defender/bazaar-profit 133, :defender/bazaar-sales 134,
   :defender/best-kill-streak 135, :defender/blood-withdrawn 136, :defender/bounties-collected 137,
   :defender/bounties-placed 138, :defender/bounties-received 139, :defender/canabis-taken 140,
   :defender/canada-travel 141, :defender/caymans-travel 142, :defender/china-travel 143,
   :defender/city-finds 144, :defender/classifieds-placed 145, :defender/clubbed-hits 146,
   :defender/company-mail-sent 147, :defender/contracts-completed 148, :defender/critical-hits 149,
   :defender/days-as-donator 150, :defender/defends-draw 151, :defender/defends-lost 152,
   :defender/defends-ran-away 153, :defender/defends-won 154, :defender/drugs-taken 155,
   :defender/dubai-travel 156, :defender/duke-contracts-completed 157, :defender/dump-finds 158,
   :defender/dump-searches 159, :defender/ecstasy-taken 160, :defender/enemies 161,
   :defender/faction-mail-sent 162, :defender/failed-busts 163, :defender/forum-posts 164,
   :defender/friend-mail-sent 165, :defender/friends 166, :defender/hawaii-travel 167,
   :defender/highest-beaten 168, :defender/items-bought 169, :defender/items-bought-abroad 170,
   :defender/items-dumped 171, :defender/items-sent 172, :defender/japan-travel 173, :defender/karma 174,
   :defender/ketamine-taken 175, :defender/largest-mug 176, :defender/last-action 177, :defender/level 178,
   :defender/logins 179, :defender/london-travel 180, :defender/lsd-taken 181,
   :defender/machine-gun-hits 182, :defender/mail-sent 183, :defender/max-life 184,
   :defender/mechanical-hits 185, :defender/medical-items-used 186, :defender/merits-bought 187,
   :defender/mexico-travel 188, :defender/mission-credits 189, :defender/missions-completed 190,
   :defender/money-mugged 191, :defender/networth 192, :defender/opium-taken 193, :defender/overdoses 194,
   :defender/pcp-taken 195, :defender/people-busted 196, :defender/personals-placed 197,
   :defender/piercing-hits 198, :defender/pistol-hits 199, :defender/points-bought 200,
   :defender/points-sold 201, :defender/refills 202, :defender/revives 203, :defender/revives-received 204,
   :defender/rifle-hits 205, :defender/rounds-fired 206, :defender/sa-travel 207, :defender/shotgun-hits 208,
   :defender/shrooms-taken 209, :defender/signup 210, :defender/slashing-hits 211, :defender/smg-hits 212,
   :defender/speed-taken 213, :defender/spouse-mail-sent 214, :defender/stat-enhancers-used 215,
   :defender/switzerland-travel 216, :defender/temp-hits 217, :defender/times-hospitalized 218,
   :defender/times-jailed 219, :defender/times-traveled 220, :defender/torn-id 221,
   :defender/total-bounty-rewards 222, :defender/total-bounty-spent 223, :defender/total-respect 224,
   :defender/trades 225, :defender/trains-received 226, :defender/vicodin-taken 227,
   :defender/viruses-coded 228, :defender/weapons-bought 229, :defender/xanax-taken 230})

#_(def train-indices (assoc (transform sp/MAP-VALS inc data-indices) :attack/count 0))

(defn data->vector [m]
  (->> (select-keys m (keys data-indices))
       ;; (select-keys (with-mod-battle-stats m) (keys data-indices))
       (sort-by (fn [[k v]] (data-indices k)))
       (mapv second)))

#_(defn train-data->vector [m]
    (->> (select-keys m (keys train-indices))
         (sort-by (fn [[k v]] (train-indices k)))
         (mapv second)))

(defn attack-data->csv* [db fname]
  (let [pairs (attack-pairs* db)]
    (with-open [f (io/writer fname)]
      (csv/write-csv f [(map (fn [k] (apply str (rest (str k))))
                             (sort-by data-indices (keys data-indices)))])
      (doseq [pair pairs]
        (csv/write-csv f [(->> pair
                               (map :player/torn-id)
                               (attack-pair-data* db)
                               data->vector)])))))

(defn attack-data->csv [db fname]
  (attack-data->csv* (-> db :conn d/db) fname))


#_(defn train-data->csv* [db fname]
    (let [pairs (attack-pairs* db)]
      (with-open [f (io/writer fname)]
        (csv/write-csv f [(map (fn [k] (apply str (rest (str k))))
                               (sort-by train-indices (keys train-indices)))])
        (doseq [pair pairs]
          (csv/write-csv f [(->> pair
                                 (map :player/torn-id)
                                 (train-data* db)
                                 train-data->vector)])))))


(defn count-attacks* [db pair]
  (d/q '[:find (count ?a) .
         :in $ ?at-id ?d-id
         :where [?at :player/torn-id ?at-id]
         [?d :player/torn-id ?d-id]
         [?a :attack/attacker ?at]
         [?a :attack/defender ?d]]
       db
       (-> pair first :player/torn-id)
       (-> pair second :player/torn-id)))


(defn format-result [[i-prob e-prob m-prob]]
  (cond (and (>= i-prob e-prob) (>= i-prob m-prob)) [:impossible i-prob]
        (>= e-prob m-prob) [:easy e-prob]
        :else [:medium m-prob]))

(defn format-results [res]
  (map format-result res))

(defn learned-difficulty* [db at-id d-ids]
  (let [data (mapv (comp #(subvec % 1) data->vector #(attack-pair-data* db [at-id %])) d-ids)
        resp @(http/get "https://tornicorn.com/difficulty"
                        {:headers {"Content-Type" "application/json"}
                         :body (json/encode {:data data})})]
    (-> resp :body (json/decode true) :data format-results)))

(defn add-difficulties [db attacker defenders]
  (let [db (-> db :conn d/db)
        diffs (map (partial difficulty* db attacker) defenders)
        w-diffs (map #(assoc %1 :difficulty %2) defenders diffs)
        groups (group-by (comp first :difficulty) w-diffs)
        _ (log/debug groups)
        unknowns (:unknown groups)
        u-diffs (learned-difficulty* db (:player/torn-id attacker) (map :player/torn-id unknowns))
        finished-groups (assoc groups :unknown (map #(assoc %1 :difficulty %2) unknowns u-diffs))]
    (into [] cat (vals finished-groups))))
