(defproject sniper "0.1.0-SNAPSHOT"
  :description "Snipe at dead code"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.7.0"]
                                  [org.clojure/tools.analyzer.jvm "0.6.7"]
                                  [prismatic/plumbing "0.4.4"]]}})
