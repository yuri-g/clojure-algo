(ns algo.merge-sort-inversions
  (:require [clojure.java.io :as io]))

(defn- left-smaller? [left-part right-part]
  (<= (first left-part) (first right-part)))

(defn- get-part-length [xs]
  (let [n (count xs)]
    (int (Math/ceil (/ n 2)))))

(defn- merge [first-half second-half inversions-total]
  (let [output []]
    (loop [current-output       output
           left-part-remainder  first-half
           right-part-remainder second-half
           current-inversions inversions-total]
      (cond
        (nil? (first left-part-remainder)) [(concat current-output right-part-remainder) current-inversions]
        (nil? (first right-part-remainder)) [(concat current-output left-part-remainder) current-inversions]
        :else
        (if (left-smaller? left-part-remainder right-part-remainder)
          (recur (conj current-output (first left-part-remainder))
                 (rest left-part-remainder)
                 right-part-remainder
                 current-inversions)

          (let [n-inversions (count left-part-remainder)]
            (recur (conj current-output (first right-part-remainder))
                   left-part-remainder
                   (rest right-part-remainder)
                   (+ n-inversions current-inversions))))))))

(defn- split-vector [xs]
  (let [n (count xs)]
    (print xs)
    (if (= n 1)
      xs
      (let [part-length (get-part-length xs)]
        (conj [] (subvec xs 0 part-length) (subvec xs part-length n))))))

(defn merge-sort-with-inversions
  ([xs]
   (let [split-xs (split-vector xs)]
     (if (<= (count xs) 1)
       [xs 0]
       (let [merge-left-result (merge-sort-with-inversions (get split-xs 0) 0)
             merge-right-result (merge-sort-with-inversions (get split-xs 1) 0)
             sorted-left (get merge-left-result 0)
             sorted-right (get merge-right-result 0)
             inversions-left (get merge-left-result 1)
             inversions-right (get merge-right-result 1)]
         (merge sorted-left sorted-right (+ inversions-left inversions-right))))))
  ([xs inversions-total]
   (let [split-xs (split-vector xs)]
     (if (<= (count xs) 1)
       [xs inversions-total]
       (let [merge-left-result (merge-sort-with-inversions (get split-xs 0) inversions-total)
             merge-right-result (merge-sort-with-inversions (get split-xs 1) inversions-total)
             sorted-left (get merge-left-result 0)
             sorted-right (get merge-right-result 0)
             inversions-left (get merge-left-result 1)
             inversions-right (get merge-right-result 1)]
         (merge sorted-left sorted-right (+ inversions-left inversions-right)))))))

(defn- file->vector [file-name]
  (let [reader (io/reader file-name)]
    (map #(Integer. %) (line-seq reader))))

(defn count-inversions
  ([file-name]
   (let [xs (file->vector file-name)]
     (merge-sort-with-inversions xs)))
  ([file-name n]
   (let [xs (take n (file->vector file-name))]
     (merge-sort-with-inversions (vec xs)))))

(def test-cases [{:input [1 3 5 2 4 6]
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

(defn- run-tests [tests]
  (for [test tests
        :let [{input :input
               output :output} test]]
    (assert (= output (get (merge-sort-with-inversions input) 1)))))

(merge-sort-with-inversions [5 4 3 2 1])
