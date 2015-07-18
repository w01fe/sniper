(defproject w01fe/sniper "0.1.0-SNAPSHOT"
  :description "Snipe at dead code"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.7.0"]]}}
  :dependencies [[org.clojure/java.classpath "0.2.2"]
                 [org.clojure/tools.analyzer.jvm "0.6.7"]
                 [prismatic/plumbing "0.4.4"]])
