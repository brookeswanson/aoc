(ns aoc.core
  (:require [clojure.set :as clj-set]
            [clojure.string :as string]))

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
