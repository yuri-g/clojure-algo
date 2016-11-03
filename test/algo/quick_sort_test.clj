(ns algo.quick-sort-test
  (:require [algo.quick-sort :as q]
            [clojure.test :as t]
            [algo.test-utils :refer :all]))

(def ^{:private true} base-test-case-path "resources/test/quick_sort")

(def ^{:private true} first-element-pivot-cases [{:path   (str base-test-case-path "/quick_sort_10.txt")
                                                  :output 25}
                                                 {:path   (str base-test-case-path "/quick_sort_100.txt")
                                                  :output 615}
                                                 {:path   (str base-test-case-path "/quick_sort_1000.txt")
                                                  :output 10297}])

(def ^{:private true} last-element-pivot-cases [{:path   (str base-test-case-path "/quick_sort_10.txt")
                                                 :output 29}
                                                {:path   (str base-test-case-path "/quick_sort_100.txt")
                                                 :output 587}
                                                {:path   (str base-test-case-path "/quick_sort_1000.txt")
                                                 :output 10184}])

(def ^{:private true} median-of-3-pivot-cases [{:path   (str base-test-case-path "/quick_sort_10.txt")
                                                :output 21}
                                               {:path   (str base-test-case-path "/quick_sort_100.txt")
                                                :output 518}
                                               {:path   (str base-test-case-path "/quick_sort_1000.txt")
                                                :output 8921}])

(defn- test-quick-sort [pivot-function test-cases]
  (doseq [test-case test-cases]
    (let [{file-path :path
           expected-comparisons :output} test-case
          {comparisons :comparisons
           sorted-xs :elements} (q/count-comparisons file-path pivot-function)]
      (t/is (= expected-comparisons comparisons))
      (t/is (is-sorted sorted-xs)))))

(t/deftest quick-sort
  (t/testing "QuickSort"
    (t/testing "with first element pivot"
      (test-quick-sort q/first-element-pivot first-element-pivot-cases))
    (t/testing "with last element pivot"
      (test-quick-sort q/last-element-pivot last-element-pivot-cases))
    (t/testing "with median of 3 pivot"
      (test-quick-sort q/median-of-3-pivot median-of-3-pivot-cases))))

