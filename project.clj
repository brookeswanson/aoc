(defproject aoc "0.1.0-SNAPSHOT"
  :description "Advent of Code"
  :url "https://bswanson.me"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[buddy/buddy-core "1.9.0"]
                 [org.clojure/clojure "1.10.0"]
                 [org.clojure/data.csv "1.0.0"]
                 [org.clojure/math.combinatorics "0.1.6"]]
  :repl-options {:init-ns aoc.core})
