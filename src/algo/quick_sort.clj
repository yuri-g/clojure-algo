(ns algo.quick-sort)

(defn partition [xs pivot]
  (let [pivot-element (get xs pivot)
        n (count xs)]
    (loop [leftmost []
           rightmost []
           i 0]
      (if (= i n)
        (conj [] leftmost rightmost)
        (if (= i pivot)
          (recur leftmost rightmost (inc i))
          (if (< (get xs i) pivot-element)
            (recur (conj leftmost (get xs i)) rightmost (inc i))
            (recur leftmost (conj rightmost (get xs i)) (inc i))))))))

(defn quick-sort [xs pivot]
  (if (<= (count xs) 1)
    xs
    (let [[left-part, right-part] (partition xs pivot)]
      (concat (quick-sort left-part pivot) [(get xs pivot)] (quick-sort right-part pivot)))))

(quick-sort [3 9 8 4 6 10 2 5 7 1] 0)

