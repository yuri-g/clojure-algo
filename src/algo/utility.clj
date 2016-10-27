(ns algo.utility
  (:require [clojure.java.io :as io]
            [clojure.string :only split :as str]))

(defn file->vector
  "Reads file at file-path line-by-line, returns of lines.
  Optionally accepts a function that will be used to map
  lines into a vector"
  ([file-path]
   (let [reader (io/reader file-path)]
     (line-seq reader)))
  ([file-path map-with]
   (let [reader (io/reader file-path)]
     (map map-with (line-seq reader)))))

(defn split-on-tabs [input-string]
  (map #(Integer. %) (str/split input-string #"\t")))

