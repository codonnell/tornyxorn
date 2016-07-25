(defproject tornyxorn "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha10"]
                 [clj-time "0.12.0"]
                 [org.clojure/tools.nrepl "0.2.11"]
                 [com.rpl/specter "0.12.0-SNAPSHOT"]
                 [org.clojure/core.async "0.2.385"]
                 [org.immutant/web "2.1.5"]
                 [http-kit "2.1.18"]
                 [com.datomic/datomic-pro "0.9.5385"
                  :exclusions [org.slf4j/slf4j-nop
                               joda-time
                               commons-codec
                               org.jboss.logging/jboss-logging]]
                 [cheshire "5.5.0"]
                 [com.stuartsierra/component "0.3.1"]
                 [environ "1.0.3"]
                 [org.postgresql/postgresql "9.4.1209"]
                 [org.clojure/tools.logging "0.3.1"]]
  :repositories {"my.datomic.com" {:url "https://my.datomic.com/repo"
                                   :username [:gpg :env/datomic_username]
                                   :password [:gpg :env/datomic_password]}}
  :plugins [[lein-environ "1.0.3"]]
  :target-path "target/%s"
  :profiles {:test {:env {:database-url "datomic:mem://tornyxorn"
                          :chan-size "100"}}
             :dev {:source-paths ["dev"]
                   :dependencies [[reloaded.repl "0.2.2"]
                                  [org.clojure/test.check "0.9.0"]]
                   :env {:database-url "datomic:mem://tornyxorn"
                         :http-port "3000"
                         :repl-port "7888"
                         :chan-size "100"}}
             :uberjar {:aot :all
                       :main tornyxorn.core}})
