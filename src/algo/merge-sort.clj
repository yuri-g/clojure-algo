(ns algo.merge-sort)

(defn- get-part-length [array]
  (let [length (count array)]
    (int (Math/ceil (/ length 2)))))

(defn split-array [array]
  (let [part-length (get-part-length array)]
    (vector (take part-length array) (drop part-length array))))

(split-array [1 2 3 4])

(defn- left-smaller? [left-part right-part]
  (print (first left-part) " < " (first right-part) "?\n")
  (< (first left-part) (first right-part)))

(defn- merge [first-half second-half]
  (let [output-length (+ (count first-half) (count second-half))
        output []]
    (loop [current-output output
           left-part-remainder first-half
           right-part-remainder second-half]
      (if (= (count current-output) output-length)
        (let []
          current-output
          )
        (let []
          (print "\n Merge recurse:\n" left-part-remainder ":" right-part-remainder " " "&" current-output "\n")
          (cond
            (nil? (first left-part-remainder))
            (let []
              (print "\n Return: " (conj current-output right-part-remainder) "\n")
              (conj current-output right-part-remainder)
              )
            (nil? (first right-part-remainder))
            (let []
              (print "\n Return: " (conj current-output left-part-remainder) "\n")
              (conj current-output left-part-remainder)
              )
            :else
            (let []
              (print "\n")
              (if (left-smaller? left-part-remainder right-part-remainder)
                (recur (conj current-output (first left-part-remainder)) (rest left-part-remainder) right-part-remainder)
                (recur (conj current-output (first right-part-remainder)) left-part-remainder (rest right-part-remainder)))))))))
          )

(defn merge-sort [input-array]
  (let [splited-array (split-array input-array)]
    (print "splitted array? =>" splited-array "\n")
    (print "first! array? =>" (first splited-array) "\n")
    (print "last! array? =>" (first splited-array) "\n")
    (if (<= (count input-array) 1)
      input-array
      (merge (merge-sort (first splited-array)) (merge-sort (rest splited-array))))))

(print "\n!!!!!!!!!!!!!!!!!!\n")

(merge-sort [6 4 7])
