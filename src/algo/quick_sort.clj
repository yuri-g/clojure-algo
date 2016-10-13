(ns algo.quick-sort
  (:require [clojure.java.io :as io]))

(defn- partition-in-place [xs l r]
  (let [pivot-element (get xs l)
        n (count xs)]
    (loop [i (inc l)
           j (inc l)
           current-partition xs]
      (if (= j r)
        (let [final-partition (swap current-partition l (dec i))]
          [(subvec final-partition l (dec i)) (subvec final-partition i n)])
        (if (< (get xs j) pivot-element)
          (recur (inc i) (inc j) (swap current-partition j i))
          (recur i (inc j) current-partition))))))

(defn- swap [xs i j]
  (-> xs
      (assoc i (xs j))
      (assoc j (xs i))))

(defn quick-sort [xs pivot total-comparisons]
  (if (<= (count xs) 1)
    {:elements xs :comparisons total-comparisons}
    (let [[left-part, right-part] (partition-in-place xs pivot (count xs))
          left-part-sorted        (quick-sort left-part 0 (count left-part))
          right-part-sorted       (quick-sort right-part 0 (count right-part))]
      {:elements    (concat
                     (:elements left-part-sorted)
                     [(get xs pivot)]
                     (:elements right-part-sorted))
       :comparisons (+ total-comparisons (:comparisons left-part-sorted) (:comparisons right-part-sorted))})))

(defn- file->vector [file-name]
  (let [reader (io/reader file-name)]
    (map #(Integer. %) (line-seq reader))))

(def ^{:private true} test-cases [{:path   "./resources/quick_sort_10.txt"
                                   :output 25}
                                  {:path   "./resources/quick_sort_100.txt"
                                   :output 615}
                                  {:path   "./resources/quick_sort_1000.txt"
                                   :output 10297}])

(defn count-comparisons
  ([file-name]
   (let [xs (file->vector file-name)]
     (quick-sort (vec xs) 0 0)))
  ([file-name n]
   (let [xs (take n (file->vector file-name))]
     (quick-sort (vec xs) 0 0))))

(defn- run-tests [tests]
  (for [test tests
        :let [{path :path
               output :output} test]]
    (assert (= output (:comparisons (count-comparisons path))))))

(run-tests test-cases)

