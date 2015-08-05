(defproject w01fe/sniper "0.1.1-SNAPSHOT"
  :description "Snipe at dead code"
  :url "https://github.com/w01fe/sniper"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.7.0"]]}}
  :dependencies [[org.clojure/java.classpath "0.2.2"]
                 [org.clojure/tools.analyzer.jvm "0.6.7"]
                 [org.clojure/tools.namespace "0.2.4"]
                 [prismatic/plumbing "0.4.4"]]
  :lein-release {:deploy-via :shell
                 :shell ["lein" "deploy" "clojars"]}
  :signing {:gpg-key "4F31760D1FB590AE"})
