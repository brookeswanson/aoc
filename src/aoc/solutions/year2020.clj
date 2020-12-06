(ns aoc.year2020
  (:require [aoc.util :as util]
            [buddy.core.hash :as b.hash]
            [buddy.core.codecs :as b.codecs]
            [clojure.set :as clj-set]
            [clojure.string :as string]))

;; day 1
(defn find-2020
  [list num check]
  (let [relevant-num (- check num)
        exists? (some #{relevant-num} list)]
    (when exists?
      (* num relevant-num))))

(defn loop-2020
  [check numbers next-step?]
  (loop [nums  numbers]
    (let [num (or (first nums) 0)
          x-nums (rest nums)
          relevant-num (- check num)
          extra-num (if next-step?
                      (loop-2020 relevant-num numbers false)
                      (find-2020 x-nums num check))]
      (cond
        (not (seq nums))
        "whoopsie"

        (and next-step?
             (number? extra-num))
        (* extra-num num)

        (number? extra-num)
        extra-num

        :default
        (recur x-nums)))))

(defn day1
  []
  (let [data (util/read-file "2020/day1.txt" read-string)]
    {:part1 (loop-2020 2020 data)
     :part2 (loop-2020 2020 data true)}))

;; day 2
(defn add-filters
  [{:keys [min max char freqs pass]
    :as data}]
  (let [min-loc (get pass (dec min))
        max-loc (get pass (dec max))
        frequency (get freqs char 0)]
    (assoc data
           :between? (<=  min frequency max)
           :meow? (and (or (= char min-loc)
                           (= char max-loc))
                       (not= min-loc max-loc)))))

(defn parse-row
  [{:keys [pass]
    :as data}]
  (-> data
      (assoc :freqs #(frequencies (into [] pass)))
      (update :min read-string)
      (update :max read-string)
      (update :char first)
      (update :pass (partial into []))
      add-filters))

(defn day2
  []
  (let [data (util/regex-split "2020/day2.txt"
                               #"([0-9]+)\-([0-9]+) ([A-Za-z]): ([A-Za-z]+)"
                               [:min :max :char :pass]
                               identity)
        processed (map parse-row data)]
    {:day1 (count (filter :between? processed))
     :day2 (count (filter :meow? processed))}))

;; day 3
(defn ->hit-or-miss
  [{:keys [right]} idx slice]
  (let [location (* right (inc idx))]
    (case (get slice (mod location (count slice)))
      \. 0
      \# 1
      0)))

(defn day3-parse
  [stuff
   {:keys [down]
    :as slope}]
  (let [slices (->> (rest stuff) (partition (or down 1)) (map last))]
    (transduce (map-indexed
                (partial ->hit-or-miss slope)) + 0 slices)))

(defn day3
  []
  (let [data (util/read-file "2020/day3.txt" (partial into []))]
    {:part1 (day3-parse {:right 3})
     :part2 (apply + (map (partial day3-parse data)
                          [{:right 1}
                           {:right 3}
                           {:right 5}
                           {:right 7}
                           {:down 2 :right 1}]))}))

;; day 4
(def necessary-keys
  #{:pid :ecl :hcl :hgt :eyr :iyr :byr})

(defn ->map [pass]
  (let [parts (clojure.string/split pass #"\s+")
        keys (into #{} (map (comp
                             keyword
                             first
                             #(clojure.string/split % #":")) parts))]
    (= (count necessary-keys)
       (count (clj-set/intersection necessary-keys keys)))))

(defn valid-height?
  [height]
  (let [[_ num unit] (re-find #"(\d+)(in|cm)" height)]
    (case unit
      "in" (<= 59 (read-string num) 76)
      "cm" (<= 150 (read-string num) 193)
      false)))

(defn ->validate [pass]
  (let [parts (clojure.string/split pass #"\s+")
        passport-fields (map #(clojure.string/split % #":") parts)
        {:strs [byr iyr eyr ecl pid hcl hgt cid]} passport-fields]
    (and (and byr iyr eyr ecl pid hcl hgt)
         (<= 1920 (read-string byr) 2002)
         (<= 2010 (read-string iyr) 2020)
         (<= 2020 (read-string eyr) 2030)
         (some #{ecl} ["amb" "blu" "brn" "gry" "grn" "hzl" "oth"])
         (re-find #"#[0-9a-f]{6}" hcl)
         (re-find #"^\d{9}$" pid)
         (valid-height? hgt))))

(defn day4
  []
  (let [data (string/split-lines (util/simple-read-file "2020/day4.txt" false) "\n\n")]
    {:part1 (->> (map ->map data) (remove identity) count)
     :part2 (->> (map ->validate data) (remove identity) count)}))

;; day 5
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
                               [:row :seat]
                               identity)
        seats (sort (into [] (comp (map ->seat)
                                   (map :seat-id))
                          data))]
    {:part1 
     (apply max seats)
     :part2 (range? seats)}))

;; day 6
(defn answers->count
  [answers]
  (->> answers
       (map (partial into #{}))
       (apply clj-set/intersection)
       count))

(defn ->day6-p2
  [data]
  (transduce (comp (map #(string/split % #"\n"))
                   (map answers->count))
             + 0 data))

(defn ->day6-p1
  [data]
  (transduce
   (comp (map #(string/replace % #"\n" ""))
         (map #(count (into #{} %))))
   + 0
   data))

(defn day6
  []
  (let [data (string/split (util/simple-read-file "2020/day6.txt" false) #"\n\n")]
    {:part1 (->day6-p1 data)
     :part2 (->day6-p2 data)}))
