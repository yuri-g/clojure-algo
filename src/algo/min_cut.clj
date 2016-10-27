(ns algo.min-cut
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [algo.utility :as util :only file->vector]
            [algo.quick-sort :as quick-sort]))

(defn split-on-tabs [input-string]
  (vec (map #(Integer. %) (str/split input-string #"\t"))))

(defrecord Vertex [label adjacent-vertices])

(defn- replace-if [new-label old-label adjacency]
  (if (= adjacency old-label)
    new-label
    adjacency))

(defn- replace-adjacency [new-label old-label vertex]
  ;(spit "./debug-3.txt" (str "Replacing" old-label " with " new-label "in: \n") :append true)
  ;(spit "./debug-3.txt" (str (:adjacent-vertices vertex) "\n") :append true)
  (let [new-adjacency (map #(replace-if new-label old-label %) (:adjacent-vertices vertex))]
    ;(spit "./debug-3.txt" (str "resuling in:" (vec new-adjacency) "\n") :append true)
    (->Vertex (:label vertex) (vec new-adjacency))))

(defn- contract-with-others [u-label v-label graph]
  (vec (map #(replace-adjacency u-label v-label %) graph)))

(defn- remove-self-loops [label adjacent-vertices]
  ;(spit "./debug-2.txt" (str "Before: " label " + " (vec adjacent-vertices) "\n") :append true)
  (vec (remove #(= label %) adjacent-vertices)))

(defn- contract-vertices [u v]
  (let [cntd (concat (:adjacent-vertices u) (:adjacent-vertices v))
        n-before (count cntd)
        without-loops (remove-self-loops (:label v) (remove-self-loops (:label u)  cntd))
        n-after (count without-loops)]
    ;(spit "./debug-2.txt" (str "After: " (:label u) " + " (vec without-loops) "\n") :append true)
    (->Vertex (:label u) (vec without-loops))))

(defn- contract [u-index v-index graph]
  (let [u (get graph u-index)
        v (get graph v-index)
        u-label (:label u)
        v-label (:label v)
        new-graph (assoc graph u-index (contract-vertices u v))]
    (contract-with-others u-label v-label (remove #(= v-label (:label %)) new-graph))))

(defn- are-connected [u v]
  (let [vertex-label (:label u)
        adjacency (:adjacent-vertices v)]
    (some #(= vertex-label %) adjacency)))

(defn- min-cut [xs]
  (loop [current-xs xs]
    (let [n (count current-xs)
          u-index (rand-int n)
          v-index (rand-int n)]
      (if (= n 2)
        current-xs
        (if (and (not (= u-index v-index)) (are-connected (get current-xs u-index) (get current-xs v-index)))
          (recur (contract u-index v-index current-xs))
          (recur current-xs))))))

(defn- make-vertex-array [xs]
  (let [[label] xs
        adjacent-vertices (vec (rest xs))]
    (->Vertex label adjacent-vertices)))

(let [min-cut-graph  (min-cut (vec (map make-vertex-array (util/file->vector "./resources/min-cut-problem.txt" split-on-tabs))))
      [first second] min-cut-graph
      first-count    (count (:adjacent-vertices first))
      second-count (count (:adjacent-vertices second))]
  (spit "result.txt" (str (vec first) "\n\n" (vec second) "\n\n First count:\n" first-count "\n Second count")))
