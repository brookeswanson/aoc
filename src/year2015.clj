(ns aoc.year2015
  (:require [aoc.util :as util]))

(defn day1
  []
  (let [data (util/simple-read-file "2015/day1.txt" false)]
    {:part1
     (let [freqs (frequencies data)
           up (get freqs \()
           down (get freqs \))]
       (- up down))
     :part2 
     (loop [[a & rest] (map-indexed vector data)
            floor 0]
       (let [[index up-down] a
             change (case up-down \( 1 \) -1 0)
             new-floor (+ floor change)]
         (if (< new-floor 0)
           (inc index)
           (recur rest new-floor))))}))

(defn ->areas
  [{:keys [l w h]}]
  (let [areas [(* l h) (* h w) (* w l)]
        xtra (apply min areas)
        total-area (apply + (map (partial * 2) areas))]
    (+ xtra total-area)))

(defn ->ribbon-feet
  [pkg]
  (let [dims (sort < (vals pkg)) 
        bow (apply * dims)
        loose (apply + (map (partial * 2) (butlast dims))) ]
    (+ bow loose)))

(defn day2
  []
  (let [data (util/regex-split "2015/day2.txt"
                               #"(\d+)x(\d+)x(\d+)"
                               [:l :w :h])]
    {:part1 (transduce (map ->areas) + 0 data)
     :part2 (transduce (map ->ribbon-feet) + 0 data)}))

(defn move
  [[x y] c]
  (case c
    \^ [x (inc y)]
    \v [x (dec y)]
    \< [(dec x) y]
    \> [(inc x) y]
    [x y]))

(defn visit-houses
  [data]
  (loop [directions data
         coordinate [0 0] 
         visited #{[0 0]}]
    (if-not (seq directions)
      visited
      (let [[a & dirs] directions
            new-coords (move coordinate a)]
        (recur dirs
               new-coords
               (conj visited new-coords))))))
(defn day3
  []
  (let [data (util/simple-read-file "2015/day3.txt" false)
        datums (partition 2 data)
        a (map first datums)
        b (map second datums)]
    {:part1 (count (visit-houses data))
     :part2 (count (into #{}
                         (concat
                          (visit-houses a)
                          (visit-houses b))))}))

(day3)
(partition 2 (util/simple-read-file "2015/day3.txt" false))
