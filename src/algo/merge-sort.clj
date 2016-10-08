(ns algo.merge-sort)

(defn merge-sort [xs]
  (let [split-xs (split-vector xs)]
    (if (<= (count xs) 1)
      xs
      (vec (merge (merge-sort (get split-xs 0)) (merge-sort (get split-xs 1)))))))

(defn- split-vector [xs]
  (let [n (count xs)]
    (if (= n 1)
      xs
      (let [part-length (get-part-length xs)]
        (conj [] (subvec xs 0 part-length) (subvec xs part-length n))))))

(defn- get-part-length [xs]
  (let [n (count xs)]
    (int (Math/ceil (/ n 2)))))

(defn- merge [first-half second-half]
  (let [output []]
    (loop [current-output       output
           left-part-remainder  first-half
           right-part-remainder second-half]
      (cond
        (nil? (first left-part-remainder)) (concat current-output right-part-remainder)
        (nil? (first right-part-remainder)) (concat current-output left-part-remainder)
        :else
        (if (left-smaller? left-part-remainder right-part-remainder)
          (recur (conj current-output (first left-part-remainder)) (rest left-part-remainder) right-part-remainder)
          (recur (conj current-output (first right-part-remainder)) left-part-remainder (rest right-part-remainder)))))))

(defn- left-smaller? [left-part right-part]
  (< (first left-part) (first right-part)))


