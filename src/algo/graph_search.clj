(ns algo.graph-search
  (:require [clojure.spec :as s]
            [clojure.spec.test :as stest]))

(s/def ::is-explored boolean?)

(s/def ::heads (s/coll-of integer? :kind vector?))
(s/def ::vertex (s/keys :req [::heads ::is-explored]))

(s/def ::vertices (s/map-of integer? ::vertex :conform-keys true))

(defn- mark-explored [vertex]
  (assoc vertex ::is-explored true))

(defn- explore [vertex vertices]
  (assoc vertices vertex (mark-explored vertex)))

(defn- get-edges-for-vertex [current-vertex vertices]
  (let [{heads ::heads} current-vertex]
    (reduce (fn [vertex-edges head]
              (if (contains? vertices head)
                (assoc vertex-edges head (get vertices head))
                vertex-edges))
            {}
            heads)))

(defn depth-first-search [vertices current-vertex]
  (let [explored-vertices (explore current-vertex vertices)
        current-explored-vertex (get explored-vertices current-vertex)
        edges (get-edges-for-vertex current-explored-vertex explored-vertices)]
    (if (empty edges)
      explored-vertices
      (for [edge edges]
        (if (not (::is-explored? edge))
          (depth-first-search explored-vertices (get explored-vertices edge)))))))

(def test-graph {1 {::heads [2, 3] ::is-explored false}
                 2 {::heads [3] ::is-explored false}
                 3 {::heads [] ::is-explored false}})

(depth-first-search test-graph (get test-graph 2))

(s/fdef depth-first-search
        :args (s/cat :vertices ::vertices
                     :starting-vertex integer?)
        :ret ::explorable-edges)

(s/fdef mark-explored
        :args (s/cat :edge ::explorable-edge)
        :ret ::explorable-edge)

(s/fdef explore
        :args (s/cat :current-vertex ::vertex
                     :vertices ::vertices))

(stest/check `depth-first-search)
(stest/check `mark-explored)
(stest/check `explore)

