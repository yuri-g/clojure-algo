(ns algo.min-cut-test
  (:require [algo.min-cut :as min-cut]
            [clojure.test :as t]))

(def ^{:private true} base-test-case-path "resources/test/min_cut")

(def ^{:private true} test-cases [{:path   (str base-test-case-path "/min_cut_test_1.txt")
                                   :output 2}
                                  {:path   (str base-test-case-path "/min_cut_test_2.txt")
                                   :output 2}
                                  {:path   (str base-test-case-path "/min_cut_test_3.txt")
                                   :output 1}
                                  {:path   (str base-test-case-path "/min_cut_test_4.txt")
                                   :output 1}
                                  {:path   (str base-test-case-path "/min_cut_test_5.txt")
                                   :output 3}
                                  {:path   (str base-test-case-path "/min_cut_test_6.txt")
                                   :output 2}])

(t/deftest min-cut-test
  (t/testing "MinCut"
    (doseq [test-case test-cases]
      (let [{file-path :path
             expected-output :output} test-case
            [min-cut-count] (sort (map (fn [_] (min-cut/min-cut file-path)) (range 0 50)))]
        (t/is (= expected-output min-cut-count))))))

