(ns tornyxorn.db
  (:require [datomic.api :as d]
            [clojure.spec :as s]
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


(defn add-basic-info-tx [info]
  (let [parsed-info (s/conform :resp/basic-info info)]
    (if-not (= ::s/invalid parsed-info)
      [(add-tempid parsed-info)]
      (throw (ex-info "Invalid basic info" (s/explain-data :resp/basic-info info))))))

(defn add-basic-info* [conn info]
  (d/transact conn (add-basic-info-tx info)))

(defn add-basic-info [db info]
  (add-basic-info* (:conn db) info))


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
