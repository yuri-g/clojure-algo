(ns algo.core
  (:gen-class)
  (:use [algo.merge-sort :as merge-sort]
        [algo.quick-sort :as quick-sort]))

(defn- print-result [algorithm subproblem file-name result]
  (if (nil? subproblem)
    (println "Result of running " algorithm " on " file-name ": " result)
    (println "Result of running " algorithm "(" subproblem ") on " file-name ": " result)))

; Add error handling
(defn -main
  [& args]
  (let [[algorithm file-name subproblem] args]
    (case algorithm
      "merge-sort" (get (merge-sort/count-inversions file-name) 1)
      "quick-sort" (case subproblem
        "first-pivot" (print-result "QuickSort" "first pivot" file-name (:comparisons (quick-sort/count-comparisons file-name quick-sort/first-element-pivot)))
        "last-pivot" (print-result "QuickSort" "last pivot" file-name (:comparisons (quick-sort/count-comparisons file-name quick-sort/last-element-pivot)))
        "median" (print-result "QuickSort" "median pivot" file-name (:comparisons (quick-sort/count-comparisons file-name quick-sort/median-of-3-pivot)))))))
