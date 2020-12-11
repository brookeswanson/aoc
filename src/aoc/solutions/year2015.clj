(ns aoc.year2015
  (:require [aoc.util :as util]
            [aoc.math :as math]
            [clojure.set :as clj-set]
            [clojure.string :as string]
            [buddy.core.hash :as buddy.hash]
            [buddy.core.codecs :as buddy.codecs]))

;; day1
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
;; day 2
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

;; day 3
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

;; day 4
(defn ->lowest-value
  [test secret start]
  (loop [ndx start]
    (let [digest (-> (str secret ndx)
                     buddy.hash/md5
                     buddy.codecs/bytes->hex)]
      (if (string/starts-with? digest test)
        ndx
        (recur (inc ndx))))))

(defn day4 []
  (let [secret "iwrupvqb"
        low-val-1 (->lowest-value "00000" secret 1)]
    {:part1 low-val-1
     :part2 (->lowest-value "000000" secret low-val-1)}))

;; day 5
(def vowels [\a \e \i \o \u])

(defn nice-2?
  [test-str]
  (and (re-find #"([a-z]{2}).*\1" test-str)
       (re-find #"([a-z])[a-z]\1" test-str)))

(defn nice?
  [test-str]
  (let [str-set (apply + (vals (select-keys (frequencies test-str) vowels)))]
    (and (not (re-find #"ab|cd|pq|xy" test-str))
         (re-find #"([a-z])\1" test-str)
         (<= 3 str-set))))

(defn day5 []
  (let [data (util/simple-read-file "2015/day5.txt")]
    {:part1 (count (filter nice? data))
     :part2 (count (filter nice-2? data))}))

;; day 6
(defn day6-p1 [commands array n]
  (doseq [{:keys [x1 y1 x2 y2 command]} commands
          x (range x1 x2)
          y (range y1 y2)
          :let [idx (+ (* n x) y)]]
    (case command
      "turn off" (aset array idx 0)
      "turn on" (aset array idx 1)
      "toggle" (aset array idx (bit-flip (aget array idx) 0)))))

(defn day6->parsed
  [{:keys [command start-x start-y end-x end-y]}]
  {:command command
   :x1 (math/->int start-x)
   :y1 (math/->int start-y)
   :x2 (inc (math/->int end-x))
   :y2 (inc (math/->int end-y))})

(defn contained?
  [{:keys [x1 y1 x2 y2] :as meow} check-box]
  (not
   (and (<= x1 (:x1 check-box))
        (<= y1 (:y1 check-box))
        (>= x2 (:x2 check-box))
        (>= x2 (:x2 check-box)))))

(defn filter-silly
  [data]
  (loop [[instruction & ins] (reverse data)
         follow-structs  []]
    (let [filtered (filter (partial contained? instruction) ins)
          new-structs (concat [instruction] follow-structs)
          command (:command instruction)]
      (if (seq ins)
        (case command
          "toggle" (recur ins new-structs)
          "turn on" (recur filtered new-structs)
          "turn off" (recur filtered new-structs))
        follow-structs))))

(defn day6 [file-name n]
  (let [data (->> (util/regex-split file-name
                                    #"(\w+\s?\w+) (\d+),(\d+) through (\d+),(\d+)"
                                    [:command :start-x :start-y :end-x :end-y]
                                    identity)
                  (map day6->parsed))
        lights (into-array (repeat (* n n) 0))
        _ (day6-p1 data lights n)
        part-1 (reduce (fn [result idx]
                         (+ (aget lights idx) result))
                       0
                       (range (alength lights)))]
    {:part1 part-1}))
