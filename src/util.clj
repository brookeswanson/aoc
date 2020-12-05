(ns aoc.util
  (:require [clojure.set :as clj-set]
            [clojure.java.io :as io]
            [clojure.string :as string]))



(defn read-file
  [file-name parse-fn]
  (->> file-name
       (io/resource)
       (slurp)
       (string/split-lines)
       (map parse-fn)))

(defn simple-read-file
  ([file-name]
   (simple-read-file file-name true))
  ([file-name split-lines]
   (cond-> (slurp (io/resource file-name))
     split-lines (string/split-lines))))

(defn regex-split
  [file-name regex map-keys]
  (read-file file-name
             (comp
              (partial zipmap map-keys)
              rest
              (partial re-find regex))))
(defn simple-loop
  [])

(defn for-loop
  [])

