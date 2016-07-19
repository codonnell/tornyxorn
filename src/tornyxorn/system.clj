(ns tornyxorn.system
  (:require [com.stuartsierra.component :as component]
            [clojure.core.async :refer [chan]]
            [immutant.util]
            [environ.core :refer [env]]
            [tornyxorn.notifier :refer [new-notifier]]
            [tornyxorn.update-handler :refer [new-update-handler]]
            [tornyxorn.torn-api :refer [new-torn-api]]
            [tornyxorn.update-creator :refer [new-update-creator]]
            [tornyxorn.handler :refer [new-handler]]
            [tornyxorn.db :refer [new-datomic-db]]
            [tornyxorn.repl :refer [new-repl-server]]
            [tornyxorn.server :refer [new-web-server]]))


(defn dev-system []
  (immutant.util/set-log-level! :DEBUG)
  (let [req-chan (chan (Integer. (env :chan-size)))
        api-chan (chan (Integer. (env :chan-size)))
        update-chan (chan (Integer. (env :chan-size)))
        notify-chan (chan (Integer. (env :chan-size)))
        ws-map (atom {})]
    (component/system-map
     :db (new-datomic-db (env :database-url))
     :repl (new-repl-server (Integer. (env :repl-port)))
     :handler (component/using
               (new-handler {:req-chan req-chan :ws-map ws-map})
               [:db])
     :update-creator (component/using
                      (new-update-creator {:req-chan req-chan
                                           :api-chan api-chan
                                           :update-chan update-chan})
                      [:db :torn-api])
     :torn-api (component/using
                (new-torn-api {:api-chan api-chan
                               :update-chan update-chan})
                [:db])
     :update-handler (component/using
                      (new-update-handler {:req-chan req-chan
                                           :update-chan update-chan
                                           :notify-chan notify-chan})
                      [:db :torn-api])
     :notifier (new-notifier {:notify-chan notify-chan
                              :ws-map ws-map})
     :http (component/using
            (new-web-server {:port (Integer. (env :http-port))})
            [:handler]))))

