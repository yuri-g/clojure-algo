(ns algo.graph-search
  (:require [clojure.spec :as s]
            [clojure.spec.test :as stest]))

(s/def ::head integer?)
(s/def ::tail integer?)
(s/def ::is-explored boolean?)

(s/def ::edge (s/keys :req [::head] :opt [::tail]))

;TODO those are not edges, but vertices + edges, rename!
(s/def ::edges (s/map-of integer? ::edge :conform-keys true))

(s/def ::explorable-edge (s/keys :req [::head ::is-explored] :opt [::tail]))

;TODO those are not edges, but vertices + edges, rename!
; also, map will not do, since keys have to be unique
; store in map, but keep array of all edges per vertex?
(s/def ::explorable-edges (s/map-of integer? ::explorable-edge :conform-keys true))

(defn- edges->explorable-edges [edges]
  (reduce (fn [explorable-edges [vertex edge]] (assoc explorable-edges vertex (assoc edge ::is-explored false))) {} edges))

(defn- mark-explored [edge]
  (assoc edge ::is-explored true))

(defn- explore [graph edge]
  (assoc graph edge (mark-explored edge)))

(defn- get-edges-for-vertex [current-vertex graph]
  (reduce (fn [edges [vertex edge]]
            (if (= vertex current-vertex)
              (assoc edges (::head edge) edge)
              edges))
          {}
          graph))

(get-edges-for-vertex 1 test-graph)

(defn depth-first-search [edges start-vertex]
  (let [explorable-edges (edges->explorable-edges edges)]
    (loop [already-explored explorable-edges
           current-edge (get already-explored start-vertex)
           current-edges (explore already-explored (::head current-edge))])))

;TODO reverse head<->tail
(def test-graph {1 {::head 2}
                 2 {::head 3}
                 3 {::head 1}})

(explore test-graph (get test-graph 1))

(s/fdef depth-first-search
        :args (s/cat :edges ::edges
                     :starting-node integer?)
        :ret ::explorable-edges)

(s/fdef edges->explorable-edges
        :args (s/cat :edges ::edges)
        :ret ::explorable-edges)

(s/fdef mark-explored
        :args (s/cat :edge ::explorable-edge)
        :ret ::explorable-edge)

(s/fdef explore
        :args (s/cat :graph ::explorable-edges
                     :edge ::explorable-edge))

(stest/check `depth-first-search)
(stest/check `edges->explorable-edges)
(stest/check `mark-explored)
(stest/check `explore)

