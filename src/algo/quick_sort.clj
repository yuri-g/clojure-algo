(ns algo.quick-sort
  (:require [clojure.java.io :as io]))

(defn- swap [xs i j]
  (-> xs
      (assoc i (xs j))
      (assoc j (xs i))))

(defn- partition-in-place [xs pivot r]
  (let [pivot-element (get xs pivot)
        n (count xs)
        xs-with-swapped-pivot (swap xs pivot 0)
        l 0]
    (loop [i (inc l)
           j (inc l)
           current-partition xs-with-swapped-pivot]
      (if (= j r)
        (let [final-partition (swap current-partition l (dec i))]
          [(subvec final-partition l (dec i)) (subvec final-partition i n)])
        (if (< (get current-partition j) pivot-element)
          (recur (inc i) (inc j) (swap current-partition j i))
          (recur i (inc j) current-partition))))))

(defn quick-sort [xs pivot-function total-comparisons]
  (if (<= (count xs) 1)
    {:elements xs :comparisons total-comparisons}
    (let [pivot (pivot-function xs)
          [left-part, right-part] (partition-in-place xs pivot (count xs))
          left-part-sorted        (quick-sort left-part pivot-function (count left-part))
          right-part-sorted       (quick-sort right-part pivot-function (count right-part))]
      {:elements    (concat
                     (:elements left-part-sorted)
                     [(get xs pivot)]
                     (:elements right-part-sorted))
       :comparisons (+ total-comparisons (:comparisons left-part-sorted) (:comparisons right-part-sorted))})))

(defn first-element-pivot [xs]
  0)

(defn last-element-pivot [xs]
  (if (= (count xs) 0)
    0
    (dec (count xs))))

(defn- get-middle-index [xs]
  (let [n (count xs)
        even-middle (/ n 2)]
    (if (even? n)
      (dec even-middle)
      (int (Math/floor even-middle)))))

(defn- median [numbers]
  (let [sort-result (quick-sort numbers first-element-pivot 0)
        ; replace with proper destructuring
        sorted-elements (vec (:elements sort-result))
        middle-index (get-middle-index sorted-elements)]
    (get sorted-elements middle-index)))

(defn median-of-3-pivot [xs]
  (if (= (count xs) 0)
    0
    (let [first 0
          middle (get-middle-index xs)
          last (dec (count xs))]
      (.indexOf xs (median [(get xs first) (get xs middle) (get xs last)])))))

(defn- file->vector [file-name]
  (let [reader (io/reader file-name)]
    (map #(Integer. %) (line-seq reader))))

(def ^{:private true} test-cases [{:path   "./resources/quick_sort_10.txt"
                                   :output 25
                                   :input first-element-pivot}
                                  {:path   "./resources/quick_sort_100.txt"
                                   :output 615
                                   :input first-element-pivot}
                                  {:path   "./resources/quick_sort_1000.txt"
                                   :output 10297
                                   :input first-element-pivot}
                                  {:path   "./resources/quick_sort_10.txt"
                                   :output 29
                                   :input last-element-pivot}
                                  {:path   "./resources/quick_sort_100.txt"
                                   :output 587
                                   :input last-element-pivot}
                                  {:path   "./resources/quick_sort_1000.txt"
                                   :output 10184
                                   :input last-element-pivot}
                                  {:path   "./resources/quick_sort_10.txt"
                                   :output 21
                                   :input median-of-3-pivot}
                                  {:path   "./resources/quick_sort_100.txt"
                                   :output 518
                                   :input median-of-3-pivot}
                                  {:path   "./resources/quick_sort_1000.txt"
                                   :output 8921
                                   :input median-of-3-pivot}])

(defn count-comparisons
  ([file-name pivot]
   (let [xs (file->vector file-name)]
     (quick-sort (vec xs) pivot 0)))
  ([file-name pivot n]
   (let [xs (take n (file->vector file-name))]
     (quick-sort (vec xs) pivot 0))))

(defn- run-tests [tests]
  (for [test tests
        :let [{path :path
               output :output
               input :input} test]]
    (let [result (:comparisons (count-comparisons path input))]
      (assert (= output result)))))

(run-tests test-cases)

