(ns algo.merge-sort
  (:require [clojure.java.io :as io]))

(defn- left-smaller? [left-part right-part]
  (<= (first left-part) (first right-part)))

(defn- get-part-length [xs]
  (let [n (count xs)]
    (int (Math/ceil (/ n 2)))))

(defn- merge-inversions [first-half second-half inversions-total]
  (loop [current-output       []
         left-part-remainder  first-half
         right-part-remainder second-half
         current-inversions inversions-total]
    (cond
      (empty? left-part-remainder) [(concat current-output right-part-remainder) current-inversions]
      (empty? right-part-remainder) [(concat current-output left-part-remainder) current-inversions]
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
                 (+ n-inversions current-inversions)))))))

(defn- split-vector [xs]
  (let [n (count xs)]
    (if (= n 1)
      xs
      (let [part-length (get-part-length xs)]
        (vector (subvec xs 0 part-length) (subvec xs part-length n))))))

(defn merge-sort-with-inversions [xs inversions-total]
  (let [[left-part right-part] (split-vector xs)]
    (if (= 1 (count xs))
      [xs inversions-total]
      (let [[sorted-left inversions-left] (merge-sort-with-inversions left-part inversions-total)
            [sorted-right inversions-right] (merge-sort-with-inversions right-part inversions-total)]
        (merge-inversions sorted-left sorted-right (+ inversions-left inversions-right))))))

(defn- file->vector [file-name]
  (let [reader (io/reader file-name)]
    (map #(Integer. %) (line-seq reader))))

(defn count-inversions
  ([file-name]
   (let [xs (file->vector file-name)]
     (merge-sort-with-inversions (vec xs) 0)))
  ([file-name n]
   (let [xs (take n (file->vector file-name))]
     (merge-sort-with-inversions (vec xs) 0))))

