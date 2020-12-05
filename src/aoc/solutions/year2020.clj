(ns aoc.year2020
  (:require [aoc.util :as util]
            [clojure.string :as string]))

(defn do-work
  [a [start end]]
  (let [value (/ (- end start) 2)]
    (case a
      \B [(+ start value) end]
      \F [start (- end value)]
      \L [start (- end value)]
      \R [(+ start value) end]
      [start end])))

(defn min-or-max
  [a]
  (case a
    \B (comp dec (partial apply max))
    \F (partial apply min)
    \L (partial apply min)
    \R (comp dec (partial apply max))
    (partial apply min)))

(defn for-loop
  [data end]
  (loop [[a & leftovers] data
         finished [0 end]]
    (if-not (seq leftovers)
      ((min-or-max (last data)) finished)
      (recur leftovers
             (do-work a finished)))))

(defn ->seat
  [{:keys [row seat]}]
  (let [r (for-loop row 128)
        s (for-loop seat 8)]
    {:row r
     :seat s
     :seat-id (+ (* 8 r) s)}))

(defn range?
  [seats]
  (let [nums (range (first seats) (last seats))]
    (first (remove nil? (map (fn [a b]
                               (when (not= a b)
                                 a)) nums seats)))))


(defn day5
  []
  (let [data (util/regex-split "2020/day5.txt"
                               #"([BF]{7})([LR]{3})"
                               [:row :seat])
        seats (sort (into [] (comp (map ->seat)
                                   (map :seat-id))
                          data))]
    {:part1 
     (apply max seats)
     :part2 (range? seats)}))

