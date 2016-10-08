(ns algo.core
  (:gen-class)
  (:use [algo.merge-sort-inversions :as inversions]))

(defn -main
  [& args]
  (let [file-name (first args)]
    (println "Number of inversions in " file-name " :" (get (inversions/count-inversions file-name) 1))))

