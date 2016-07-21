(ns tornyxorn.db
  (:require [datomic.api :as d]
            [clojure.spec :as s]
            [clojure.set :as set]
            [clj-time.core :as t]
            [clj-time.coerce :refer [to-date from-date]]
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
  (d/q '[:find [?k ...] :where [_ :player/api-key ?k]] db))

(defn api-keys [db]
  (api-keys* (-> db :conn d/db)))


(defn add-player-tx [player]
  [(add-tempid player)])

(defn add-player* [conn player]
  (d/transact conn (add-player-tx player)))

(defn add-player [db player]
  (add-player* (:conn db) player))


(defn add-player-info-tx [info]
  (let [parsed-info (s/conform :resp/player-info info)]
    (if-not (= ::s/invalid parsed-info)
      [(assoc parsed-info
              :player/last-player-info-update (to-date (t/now))
              :db/id (d/tempid :db.part/user))]
      (throw (ex-info "Invalid player info" (s/explain-data :resp/player-info info))))))

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

(defn schema-attack->db-attack [attack]
  (as-> attack a
    (assoc a
           :attack/attacker {:player/torn-id (:attack/attacker attack)}
           :attack/defender {:player/torn-id (:attack/defender attack)}
           :attack/result [:db/ident (:attack/result attack)])
    (update a :attack/timestamp-started to-date)
    (update a :attack/timestamp-ended to-date)
    (if (nil? (:attack/attacker attack))
      (dissoc a :attack/attacker)
      a)))

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
    :player/api-key api-key}])

(defn add-api-key* [conn api-key]
  (d/transact conn (add-api-key-tx api-key)))

(defn add-api-key [db api-key]
  (add-api-key* (:conn db) api-key))


(defn remove-api-key* [conn api-key]
  (let [player-entid (d/q '[:find ?p . :in $ ?k :where [?p :player/api-key ?k]] (d/db conn))]
    (d/transact conn [[:db/retract player-entid :player/api-key api-key]])))

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

(defn difficulty* [db a-id d-id]
  "Returns :easy if someone with fewer total stats than a-id beat d-id, :hard if
  someone with more total stats than a-id lost to d-id, :medium of both of these
  are true, and :unknown if neither are true."
  (let [attacker (player-by-id* db a-id)
        attacks-on-d (map (fn [[a tx]] (d/entity (d/as-of db tx) a))
                          (d/q '[:find ?a ?tx1
                                 :in $ ?id
                                 :where
                                 [?a :attack/defender ?d ?tx1]
                                 [?d :player/torn-id ?id]
                                 [?a :attack/attacker ?ap]
                                 [?ap :player/strength _ ?tx2]
                                 [(< ?tx2 ?tx1)]]
                               db d-id))
        easy-attacks (filter (partial easy-attack? attacker) attacks-on-d)
        hard-attacks (filter (partial hard-attack? attacker) attacks-on-d)]
    (cond
      (and (empty? easy-attacks) (empty? hard-attacks)) :unknown
      (empty? hard-attacks) :easy
      (empty? easy-attacks) :hard
      :else :medium)))

(defn difficulty [db a-id d-id]
  (difficulty* (-> db :conn d/db) a-id d-id))

(defn difficulties* [db a-id d-ids]
  (mapv (partial difficulty* db a-id) d-ids))

(defn difficulties [db a-id d-ids]
  (difficulties* (-> db :conn d/db) a-id d-ids))
