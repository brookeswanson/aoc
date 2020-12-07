(ns aoc.math)

(defn ->int
  [num]
  (try
    (cond (string? num) (Integer/parseInt num)
          (number? num) num)
    (catch Exception e
      (println "In ->int trying to parse: " num)
        nil)))

(defn abs
  [n]
  (max n (* -1 n)))

(defn coll->max
  [coll]
  (apply max (into [] coll)))

(defn coll->min
  [coll]
  (apply min (into [] coll)))

(defn sum
  [coll]
  (apply + (into [] coll)))

(defn product
  [coll]
  (apply * (into [] coll)))

(defn avg
  [coll]
  (if (seq coll)
    (let [total (float (count coll))]
      (/ (sum coll) total))
    (println "Error calculating average. Empty list passed in.")))
