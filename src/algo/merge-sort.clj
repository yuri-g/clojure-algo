(ns algo.merge-sort)

(defn- get-part-length [array]
  (let [length (count array)]
    (int (Math/ceil (/ length 2)))))

(defn split-array [array]
  (if (= (count array) 1)
    array
    (let [part-length (get-part-length array)]
      (conj [] (subvec array 0 part-length) (subvec array part-length (count array))))))

(defn- left-smaller? [left-part right-part]
  (< (first left-part) (first right-part)))

(defn- merge [first-half second-half]
  (let [output-length (+ (count first-half) (count second-half))
        output        []]
    (loop [current-output       output
           left-part-remainder  first-half
           right-part-remainder second-half]
      (if (= (count current-output) output-length)
        current-output
          (cond
            (nil? (first left-part-remainder)) (concat current-output right-part-remainder)
            (nil? (first right-part-remainder)) (concat current-output left-part-remainder)
            :else
            (if (left-smaller? left-part-remainder right-part-remainder)
              (recur (conj current-output (first left-part-remainder)) (rest left-part-remainder) right-part-remainder)
              (recur (conj current-output (first right-part-remainder)) left-part-remainder (rest right-part-remainder))))))))

(defn merge-sort [input-array]
  (let [splited-array (split-array input-array)]
    (if (<= (count input-array) 1)
      input-array
      (vec (merge (merge-sort (get splited-array 0)) (merge-sort (get splited-array 1)))))))

(merge-sort [0 89 0 2 0 89 1 8 77 212 9 5.4 89])

