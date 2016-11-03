(ns algo.merge-sort-test
  (:require [algo.merge-sort :as m]
            [clojure.test :as t]
            [algo.test-utils :refer :all]))

(def ^{:private true} test-cases [{:input [1 3 5 2 4 6]
                                   :output 3}
                                  {:input [1 5 3 2 4]
                                   :output 4}
                                  {:input [5 4 3 2 1]
                                   :output 10}
                                  {:input [1 6 3 2 4 5]
                                   :output 5}
                                  {:input [9 12 3 1 6 8 2 5 14 13 11 7 10 4 0]
                                   :output 56}
                                  {:input [37 7 2 14 35 47 10 24 44 17 34 11 16 48 1 39 6 33 43 26
                                           40 4 28 5 38 41 42 12 13 21 29 18 3 19 0 32 46 27 31 25
                                           15 36 20 8 9 49 22 23 30 45]
                                   :output 590}
                                  {:input [4 80 70 23 9 60 68 27 66 78 12 40 52 53 44 8 49 28 18 46
                                           21 39 51 7 87 99 69 62 84 6 79 67 14 98 83 0 96 5 82 10
                                           26 48 3 2 15 92 11 55 63 97 43 45 81 42 95 20 25 74 24
                                           72 91 35 86 19 75 58 71 47 76 59 64 93 17 50 56 94 90
                                           89 32 37 34 65 1 73 41 36 57 77 30 22 13 29 38 16 88
                                           61 31 85 33 54]
                                   :output 2372}])

(t/deftest merge-sort-test
  (t/testing "MergeSort"
    (doseq [test-case test-cases
          :let [{input :input
                 expected-output :output} test-case
                [sorted-xs n-inversions] (m/merge-sort-with-inversions input 0)]]
      (t/is (and (is-sorted sorted-xs) (= expected-output n-inversions))))))

