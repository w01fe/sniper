(ns sniper.snarf-test
  (:use clojure.test)
  (:require
   [clojure.tools.analyzer.jvm :as jvm]
   [sniper.snarf :as snarf]))

(deftest definterface-test
  (binding [*ns* (the-ns 'sniper.snarf-test)]
    (is (= {:class-defs ["sniper.snarf_test.Foo"] :var-defs []}
           (select-keys
            (snarf/normalized-form
             (jvm/analyze '(definterface Foo (bar [this]))))
            [:class-defs :var-defs])))))

(deftest defprotocol-test
  (binding [*ns* (the-ns 'sniper.snarf-test)]
    (is (= {:class-defs ["sniper.snarf_test.Foo"],
            :var-defs ['sniper.snarf-test/Foo 'sniper.snarf-test/bar]}
           (select-keys
            (snarf/normalized-form
             (jvm/analyze '(defprotocol Foo (bar [this]))))
            [:class-defs :var-defs])))))
