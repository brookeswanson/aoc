(ns aoc.year2016
  (:require [aoc.util :as util]
            [clojure.set :as clj-set]
            [clojure.string :as string]
            [buddy.core.hash :as buddy.hash]
            [buddy.core.codecs :as buddy.codecs]))

;; day1
(def cardinalities {:west {\R :north
                           \L :south
                           :mult [-1 0]}
                    :east {\R :south
                           \L :north
                           :mult [1 0]}
                    :south {\R :west
                            \L :east
                            :mult [0 -1]}
                    :north {\R :east
                            \L :west
                            :mult [0 1]}})

(defn dirs->block
  [cardinality [_ direction blocks]]
  (let [dirs (get cardinalities cardinality)
        dir (first direction)
        new-dir (get dirs dir) 
        block (Integer/parseInt blocks)
        blocks (into []
                   (map #(* block (int %)))
                   (get-in cardinalities [new-dir :mult]))]
    {:next-cardinality new-dir
     :blocks blocks
     :seen-coords []}))

(defn abs
  [n]
  (max n (* -1 n)))

(defn drive
  [coll]
  (loop [[dir & directions] coll
         cardinality :north
         [up right] [0 0]]
    (let [{:keys [next-cardinality blocks]} (dirs->block cardinality dir)
          [next-up next-right] blocks
          new-coords [(+ up next-up)
                      (+ right next-right)]]
      (if-not (seq directions)
        (apply + (map abs new-coords))
        (recur directions next-cardinality new-coords)))))

(defn visit-twice
  [coll]
  (loop [[dir & directions] coll
         cardinality :north
         [up right] [0 0]
         visited [[0 0]]]
    (let [{:keys [next-cardinality blocks]} (dirs->block cardinality dir)
          [next-up next-right] blocks
          new-coords [(+ up next-up)
                      (+ right next-right)]]
      (if (or (not (seq directions))
              (some #{new-coords} visited))
        (apply + (map abs new-coords))
        (recur directions
               next-cardinality
               new-coords
               (conj visited new-coords))))))

(defn day1
  []
  (let [data (->> (string/split (util/simple-read-file "2016/day1.txt" false) #",\s+")
                  (map #(re-find #"([R|L])(\d+)" %)))]
    {:part1 (drive data)
     :part2 (visit-twice data)}))
