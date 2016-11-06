(ns algo.graph-search
  (:require [clojure.spec :as s]
            [clojure.spec.test :as stest]))

(s/def ::head integer?)
(s/def ::tail integer?)

(s/def ::edge (s/keys :req [::head ::tail]))
(s/def ::edges (s/map-of integer? ::edge :conform-keys true))

(defn depth-first [graph starting-node])

(s/fdef depth-first
        :args (s/cat :graph ::edges
                     :starting-node integer?))

