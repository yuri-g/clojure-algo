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
  (let [new-adjacency (map #(replace-if new-label old-label %) (:adjacent-vertices vertex))]
    (->Vertex (:label vertex) (vec new-adjacency))))

(defn- vertex->vertex-map-entry [vertex]
  [(:label vertex) vertex])

(spit "debug.txt" "")

(defn- contract-with-others [u-label v-label graph]
  (let [contracted (map #(replace-adjacency u-label v-label (get % 1)) (seq graph))
        graph-representation (apply array-map (flatten (map vertex->vertex-map-entry contracted)))]
    graph-representation))

(defn- get-result [_]
  (let [min-cut-result (min-cut (apply array-map (flatten (map vector->vertex-map-entry (util/file->vector "./resources/min-cut-problem.txt" split-on-tabs)))))]
    (count (:adjacent-vertices (get (first min-cut-result) 1)))))

(spit "result.txt" (sort (map get-result (range 0 100))))

(defn- remove-self-loops [label adjacent-vertices]
  (vec (remove #(= label %) adjacent-vertices)))

(defn- contract-vertices [u v]
  (let [cntd (concat (:adjacent-vertices u) (:adjacent-vertices v))
        n-before (count cntd)
        without-loops (remove-self-loops (:label v) (remove-self-loops (:label u)  cntd))
        n-after (count without-loops)]
    (->Vertex (:label u) (vec without-loops))))

(defn- contract [u-vertex v-vertex graph]
  (let [u-label (:label u-vertex)
        v-label (:label v-vertex)
        new-graph (assoc graph u-label (contract-vertices u-vertex v-vertex))
        removed-result (dissoc new-graph v-label)]
    (contract-with-others u-label v-label removed-result)))

(defn- random-key [xs]
  (let [xs-keys (keys xs)]
    (rand-nth xs-keys)))

(defn- min-cut [xs]
  (loop [current-xs xs]
    (let [n (count current-xs)
          u-index (random-key current-xs)
          u-vertex (u-index current-xs)
          v-index (rand-nth (:adjacent-vertices u-vertex))]
      (if (= n 2)
        current-xs
        ;(if (= u-index v-index)
          ; try again, sorry
          ;(recur current-xs)
          (recur (contract u-vertex (v-index current-xs) current-xs))))))

(defn- int->key [i]
  (keyword (str i)))

(defn- vector->vertex-map-entry [xs]
  (let [[label] xs
        adjacent-vertices (vec (map int->key (rest xs)))]
    [(keyword (str label)) (->Vertex (int->key label) adjacent-vertices)]))

