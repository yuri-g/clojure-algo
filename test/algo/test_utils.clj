(ns algo.test-utils
  (:require  [clojure.test :as t]))

(defn is-sorted [xs]
  (let [sorted-xs (sort xs)]
    (t/is (= sorted-xs xs))))

