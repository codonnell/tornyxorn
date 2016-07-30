(ns tornyxorn.db
  (:require [datomic.api :as d]
            [clojure.spec :as s]
            [clojure.set :as set]
            [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [clj-time.core :as t]
            [clj-time.coerce :refer [to-date from-date to-long]]
            [com.rpl.specter :as sp]
            [com.rpl.specter.macros :refer [select transform]]
            [com.stuartsierra.component :as component]))

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




(defn add-tempid [data]
  (assoc data :db/id (d/tempid :db.part/user)))


(defn has-player-info?* [db torn-id]
  (not (nil? (d/q '[:find ?i .
                    :in $ ?id
                    :where [?p :player/torn-id ?id] [?p :player/last-player-info-update ?i]]
                  db torn-id))))

(defn has-player-info? [db torn-id]
  (has-player-info?* (-> db :conn d/db) torn-id))


(defn player-by-id* [db torn-id]
  (d/entity db (d/q '[:find ?p . :in $ ?id :where [?p :player/torn-id ?id]] db torn-id)))

(defn player-by-id [db torn-id]
  (player-by-id* (-> db :conn d/db) torn-id))


(defn player-by-api-key* [db api-key]
  (d/entity db (d/q '[:find ?p . :in $ ?k :where [?p :player/api-key ?k]] db api-key)))

(defn player-by-api-key [db api-key]
  (player-by-api-key* (-> db :conn d/db) api-key))


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
                (map (partial d/entity db)))]
    (let [k (count ps)]
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
             (concat ps))))))

(defn stale-players [db n]
  (stale-players* (-> db :conn d/db) n))


(defn add-player-tx [player]
  [(add-tempid player)])

(defn add-player* [conn player]
  (d/transact conn (add-player-tx player)))

(defn add-player [db player]
  (add-player* (:conn db) player))


(defn remove-nils [m]
  (into {} (filter (fn [[k v]] (not (nil? v)))) m))

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


;; TODO: This needs a torn-id or api-key to work
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

(defn add-attacks-tx [attacks]
  (mapv (comp add-tempid schema-attack->db-attack) attacks))

(defn add-attacks* [conn attacks]
  (d/transact conn (add-attacks-tx attacks)))

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
  (d/q '[:find ?k .
         :in $ ?id
         :where [?p :player/torn-id ?id] [?p :player/api-key ?k]]))

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
  (let [player-entid (d/q '[:find ?p . :in $ ?k :where [?p :player/api-key ?k]] (d/db conn) api-key)
        temp-entids (d/q '[:find [?p ...] :in $ ?k :where [?p :player/temp-api-key ?k]] (d/db conn) api-key)]
    (d/transact conn (filterv (fn [[_ entid _ _]] entid)
                              (conj (mapv (fn [entid]
                                            [:db/retract entid :player/temp-api-key api-key])
                                          temp-entids)
                                    [:db/retract player-entid :player/api-key api-key])))))

(defn remove-api-key [db api-key]
  (remove-api-key* (:conn db) api-key))

;; Difficulty computation functions

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

(defn easy-attack? [attacker attack]
  (and (attack-success? attack)
       (<= (total-stats (:attack/attacker attack))
           (total-stats attacker))))

(defn hard-attack? [attacker attack]
  (and (attack-fail? attack)
       (>= (total-stats (:attack/attacker attack))
           (total-stats attacker))))

(defn difficulty* [db attacker defender-id]
  "Returns :easy if someone with fewer total stats than attacker beat defender,
  :hard if someone with more total stats than attacker lost to defender, :medium
  of both of these are true, and :unknown if neither are true."
  (let [attacks-on-d (map (fn [[a tx]] (d/entity (d/as-of db tx) a))
                          (d/q '[:find ?a ?tx1
                                 :in $ ?id
                                 :where
                                 [?a :attack/defender ?d ?tx1]
                                 [?d :player/torn-id ?id]
                                 [?a :attack/attacker ?ap]
                                 [?ap :player/strength _ ?tx2]
                                 [(< ?tx2 ?tx1)]]
                               db defender-id))
        easy-attacks (filter (partial easy-attack? attacker) attacks-on-d)
        hard-attacks (filter (partial hard-attack? attacker) attacks-on-d)]
    (cond
      (and (empty? easy-attacks) (empty? hard-attacks)) :unknown
      (empty? hard-attacks) :easy
      (empty? easy-attacks) :impossible
      :else :medium)))

(defn difficulty [db attacker d-id]
  (difficulty* (-> db :conn d/db) attacker d-id))

(defn difficulties* [db attacker d-ids]
  (mapv (partial difficulty* db attacker) d-ids))

(defn difficulties [db attacker d-ids]
  (difficulties* (-> db :conn d/db) attacker d-ids))

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

(defn transform-player [m role]
  (as-> (into {} m) m
    (apply dissoc m to-remove)
    (update m :player/signup (comp to-long from-date))
    (update m :player/last-action (comp to-long from-date))
    (change-ns m role)))

(defn pct-wins [as]
  (double (/ (count (filter #(identical? :attack/win (-> % :attack/result success-map)) as)) (count as))))

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
        attacker (transform-player (:attack/attacker (first attacks)) "attacker")
        defender (transform-player (:attack/defender (first attacks)) "defender")
        win-chance (pct-wins attacks)]
    (merge attacker defender {:attack/win-chance win-chance})))

(defn attack-pair-data [db pair]
  (attack-pair-data* (-> db :conn d/db) pair))



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
  (sorted-map :attacker/revives-received 0, :attacker/total-respect 1, :attacker/people-busted 2, :attacker/bounties-collected 3, :attacker/spouse-mail-sent 4, :attacker/argentina-travel 5, :defender/logins 6, :attacker/trains-received 7, :attacker/attacks-lost 8, :defender/vicodin-taken 9, :attacker/highest-beaten 10, :attacker/merits-bought 11, :attacker/best-kill-streak 12, :attack/win-chance 13, :defender/awards 14, :attacker/faction-mail-sent 15, :attacker/bazaar-sales 16, :attacker/bounties-placed 17, :defender/bounties-received 18, :defender/critical-hits 19, :attacker/networth 20, :defender/items-bought 21, :attacker/artillery-hits 22, :attacker/bazaar-profit 23, :attacker/mission-credits 24, :defender/defends-draw 25, :attacker/company-mail-sent 26, :defender/shotgun-hits 27, :attacker/karma 28, :defender/mail-sent 29, :defender/defends-ran-away 30, :attacker/pcp-taken 31, :defender/duke-contracts-completed 32, :attacker/dump-finds 33, :attacker/enemies 34, :attacker/dubai-travel 35, :defender/largest-mug 36, :attacker/attacks-won 37, :attacker/speed-taken 38, :defender/trades 39, :defender/rounds-fired 40, :attacker/pistol-hits 41, :attacker/ecstasy-taken 42, :attacker/defends-lost 43, :attacker/missions-completed 44, :defender/items-sent 45, :attacker/contracts-completed 46, :attacker/rifle-hits 47, :attacker/shrooms-taken 48, :attacker/refills 49, :attacker/strength 50, :defender/torn-id 51, :attacker/london-travel 52, :defender/xanax-taken 53, :attacker/viruses-coded 54, :defender/auctions-sold 55, :defender/piercing-hits 56, :attacker/japan-travel 57, :defender/items-bought-abroad 58, :attacker/mexico-travel 59, :defender/points-bought 60, :attacker/bails-spent 61, :attacker/auctions-won 62, :attacker/attacks-ran-away 63, :attacker/blood-withdrawn 64, :defender/revives 65, :defender/mechanical-hits 66, :defender/personals-placed 67, :attacker/medical-items-used 68, :attacker/total-bounty-rewards 69, :attacker/weapons-bought 70, :defender/opium-taken 71, :defender/defends-won 72, :defender/times-jailed 73, :attacker/activity 74, :attacker/attacks-stealthed 75, :defender/level 76, :attacker/canada-travel 77, :attacker/classifieds-placed 78, :defender/city-finds 79, :defender/china-travel 80, :defender/bails-bought 81, :attacker/auctions-sold 82, :attacker/torn-id 83, :defender/clubbed-hits 84, :defender/times-traveled 85, :attacker/personals-placed 86, :defender/sa-travel 87, :defender/revives-received 88, :defender/people-busted 89, :attacker/temp-hits 90, :defender/max-life 91, :defender/hawaii-travel 92, :attacker/caymans-travel 93, :attacker/failed-busts 94, :defender/machine-gun-hits 95, :defender/drugs-taken 96, :attacker/bazaar-customers 97, :defender/overdoses 98, :attacker/dump-searches 99, :attacker/total-bounty-spent 100, :defender/friend-mail-sent 101, :defender/ketamine-taken 102, :attacker/defends-draw 103, :attacker/items-bought 104, :defender/bounties-collected 105, :attacker/last-action 106, :defender/attacks-assisted 107, :attacker/logins 108, :defender/lsd-taken 109, :defender/switzerland-travel 110, :attacker/money-mugged 111, :attacker/days-as-donator 112, :defender/smg-hits 113, :attacker/stat-enhancers-used 114, :defender/slashing-hits 115, :attacker/canabis-taken 116, :defender/signup 117, :defender/items-dumped 118, :attacker/critical-hits 119, :defender/japan-travel 120, :attacker/lsd-taken 121, :attacker/shotgun-hits 122, :defender/attacks-ran-away 123, :attacker/opium-taken 124, :defender/medical-items-used 125, :attacker/signup 126, :defender/karma 127, :defender/total-respect 128, :attacker/times-jailed 129, :defender/trains-received 130, :defender/spouse-mail-sent 131, :defender/argentina-travel 132, :attacker/duke-contracts-completed 133, :defender/attacks-won 134, :attacker/largest-mug 135, :defender/merits-bought 136, :defender/friends 137, :attacker/rounds-fired 138, :defender/best-kill-streak 139, :attacker/awards 140, :defender/faction-mail-sent 141, :attacker/speed 142, :defender/bazaar-sales 143, :defender/refills 144, :defender/times-hospitalized 145, :defender/missions-completed 146, :attacker/points-bought 147, :attacker/hawaii-travel 148, :defender/viruses-coded 149, :attacker/piercing-hits 150, :attacker/bounties-received 151, :attacker/items-bought-abroad 152, :defender/bounties-placed 153, :defender/attacks-draw 154, :defender/london-travel 155, :attacker/xanax-taken 156, :defender/bails-spent 157, :defender/activity 158, :defender/attacks-stealthed 159, :defender/bazaar-profit 160, :attacker/defends-won 161, :defender/artillery-hits 162, :defender/weapons-bought 163, :attacker/revives 164, :attacker/mechanical-hits 165, :attacker/mail-sent 166, :attacker/china-travel 167, :attacker/defends-ran-away 168, :attacker/trades 169, :defender/points-sold 170, :attacker/vicodin-taken 171, :defender/attacks-lost 172, :defender/dubai-travel 173, :defender/speed-taken 174, :defender/forum-posts 175, :defender/defends-lost 176, :attacker/items-sent 177, :defender/shrooms-taken 178, :defender/pistol-hits 179, :defender/rifle-hits 180, :attacker/forum-posts 181, :attacker/switzerland-travel 182, :defender/money-mugged 183, :attacker/points-sold 184, :attacker/max-life 185, :defender/failed-busts 186, :defender/mexico-travel 187, :defender/caymans-travel 188, :defender/auctions-won 189, :defender/temp-hits 190, :attacker/overdoses 191, :defender/blood-withdrawn 192, :defender/total-bounty-rewards 193, :defender/dump-searches 194, :attacker/drugs-taken 195, :defender/bazaar-customers 196, :attacker/bails-bought 197, :defender/dump-finds 198, :defender/pcp-taken 199, :defender/classifieds-placed 200, :attacker/city-finds 201, :attacker/defense 202, :attacker/attacks-draw 203, :attacker/times-traveled 204, :attacker/clubbed-hits 205, :defender/enemies 206, :attacker/level 207, :attacker/sa-travel 208, :defender/ecstasy-taken 209, :defender/contracts-completed 210, :defender/networth 211, :attacker/attacks-assisted 212, :attacker/friends 213, :defender/days-as-donator 214, :defender/last-action 215, :attacker/slashing-hits 216, :defender/company-mail-sent 217, :defender/stat-enhancers-used 218, :defender/mission-credits 219, :attacker/times-hospitalized 220, :attacker/machine-gun-hits 221, :attacker/smg-hits 222, :defender/canabis-taken 223, :defender/highest-beaten 224, :attacker/dexterity 225, :defender/total-bounty-spent 226, :defender/canada-travel 227, :attacker/ketamine-taken 228, :attacker/friend-mail-sent 229, :attacker/items-dumped 230))

(defn data->vector [m]
  (->> (set/rename-keys m data-indices)
       (mapcat identity)
       (apply sorted-map)
       vals))

(defn attack-data->csv* [db fname]
  (let [pairs (attack-pairs* db)]
    (with-open [f (io/writer fname)]
      (csv/write-csv f (map (fn [k] (apply str (rest (str k))))
                            (sort-by data-indices (keys data-indices))))
      (doseq [pair pairs]
        (csv/write-csv f [(->> pair
                               (map :player/torn-id)
                               (attack-pair-data* db)
                               data->vector)])))))

(defn attack-data->csv [db fname]
  (attack-data->csv* (-> db :conn d/db) fname))
