(ns algo.graph-search
  (:require [clojure.spec :as s]
            [clojure.spec.test :as stest]))

(s/def ::is-explored boolean?)

(s/def ::heads (s/coll-of integer? :kind vector?))
(s/def ::vertex (s/keys :req [::heads ::is-explored]))

(s/def ::vertices (s/map-of integer? ::vertex :conform-keys true))

(defn- mark-explored [vertex]
  (assoc vertex ::is-explored true))

(defn- explore [index vertices]
  (assoc vertices index (mark-explored (get vertices index))))

(defn- get-edges [index vertices]
  (let [vertex (get vertices index)
        {heads ::heads} vertex]
    (reduce (fn [vertex-edges head]
              (if (contains? vertices head)
                (assoc vertex-edges head (get vertices head))
                vertex-edges))
            {}
            heads)))

(defn breadth-first-search [start-vertices index]
  (loop [vertices (explore index start-vertices)
         q (conj (clojure.lang.PersistentQueue/EMPTY) index)
         all vertices]
    (if (or (empty? q) (nil? (peek q)))
      all
      (let [v (peek q)
            q-updated (pop q)
            edges (get-edges v vertices)
            first (-> edges first first)]
        (if (nil? first)
          all
          (recur (explore first edges) (conj q-updated first) (into all (explore first edges))))))))

(breadth-first-search test-graph-2 2)

(-> test-graph first first)

(defn depth-first-search [vertices current-vertex]
  (let [explored-vertices (explore current-vertex vertices)
        current-explored-vertex (get explored-vertices current-vertex)
        edges (get-edges-for-vertex current-explored-vertex explored-vertices)]
    (if (empty edges)
      explored-vertices
      (for [edge edges]
        (if (not (::is-explored? edge))
          (depth-first-search explored-vertices (get explored-vertices edge)))))))

(into test-graph test-graph-2)
(def test-graph {1 {::heads [2 3] ::is-explored false}
                 2 {::heads [3] ::is-explored false}
                 3 {::heads [] ::is-explored false}})

(def test-graph-2 {1 {::heads [2 3] ::is-explored false}
                   2 {::heads [4 5] ::is-explored false}
                   3 {::heads [5] ::is-explored false}
                   4 {::heads [6] ::is-explored false}
                   5 {::heads [] ::is-explored false}
                   6 {::heads [] ::is-explored false}
                   7 {::heads [5] ::is-explored false}})

(depth-first-search test-graph (get test-graph 2))

(s/fdef depth-first-search
        :args (s/cat :vertices ::vertices
                     :starting-vertex integer?)
        :ret ::vertices)

(s/fdef mark-explored
        :args (s/cat :edge ::explorable-edge)
        :ret ::explorable-edge)

(s/fdef explore
        :args (s/cat :current-vertex ::vertex
                     :vertices ::vertices))

(stest/check `depth-first-search)
(stest/check `mark-explored)
(stest/check `explore)

