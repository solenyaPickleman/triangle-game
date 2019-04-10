(defproject triangle_game "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"] [org.clojure/clojure-contrib "1.2.0"] [org.clojure/data.zip "0.1.3"]]
  :main ^:skip-aot triangle-game.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
