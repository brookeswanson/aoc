(ns aoc.core
  (:require [clojure.set :as clj-set]
            [clojure.string :as string]))

(def day3 (->> (slurp "day3.txt")
               (clojure.string/split-lines)
               (map (partial into []))))

(def day4 (-> (slurp "day4.txt")
              (clojure.string/split #"\n\n")))

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
        {:keys [byr iyr eyr ecl pid hcl hgt cid]}
        (into {}
              (map (fn [[k v]]
                     [(keyword k) v]))
              passport-fields)]
    (and (and byr iyr eyr ecl pid hcl hgt)
         (<= 1920 (read-string byr) 2002)
         (<= 2010 (read-string iyr) 2020)
         (<= 2020 (read-string eyr) 2030)
         (some #{ecl} ["amb" "blu" "brn" "gry" "grn" "hzl" "oth"])
         (re-find #"#[0-9a-f]{6}" hcl)
         (re-find #"^\d{9}$" pid)
         (valid-height? hgt))))

(defn hit?
  [test-car]
  (= test-car \#))

(defn ->hit-or-miss
  [{:keys [right]} idx slice]
  (let [location (* right (inc idx))]
    (case (get slice (mod location (count slice)))
      \. 0
      \# 1
      0)))

(defn day3-transduced
  [stuff
   {:keys [down]
    :as slope}]
  (let [slices (->> (rest stuff) (partition (or down 1)) (map last))]
    (transduce (map-indexed
                (partial ->hit-or-miss slope)) + 0 slices)))

(defn day3-stuff
  [stuff
   {:keys [right down]}]
  (let [mod-num (count (first stuff))
        starting-remaining (->> (rest stuff) (partition (or down 1)) (map last))]
    (loop [hit-count 0
           remaining starting-remaining
           curr-pos 0]
      (if (not (seq remaining))
        hit-count
        (let [curr-level (first remaining)
              leftovers (rest remaining)
              new-level (+ curr-pos right)
              test-car (get curr-level (mod new-level mod-num))]
          (recur (if (hit? test-car) (inc hit-count) hit-count)
                 leftovers
                 new-level))))))

(defn add-meow?
  [{:keys [min max char location]
    :as data}]
  (let [min-loc (get location (dec min))
        max-loc (get location (dec max))]
    (assoc data
           :meow?
           (and (or (= char min-loc)
                    (= char max-loc))
                (not= min-loc max-loc)))))

(defn add-between?
  [{:keys [min max char pass]
    :as data}]
  (let [frequency (get pass char 0)]
    (assoc data :between? (<=  min frequency max))))

(defn parse-row
  [row]
  (let [[_ min max char pass] (re-find #"([0-9]+)\-([0-9]+) ([A-Za-z]): ([A-Za-z]+)" row)
        letters (frequencies (into [] pass))
        location (into [] pass)
        relevant-char (first (into #{} char))]
    (-> {:min (read-string min)
         :max (read-string max)
         :char relevant-char 
         :location location
         :pass letters}
        add-between?
        add-meow?)))

(defn day2
  []
  (->> (slurp "day2.txt")
       (clojure.string/split-lines)
       (map parse-row)
       (filter :meow?)
       count))
