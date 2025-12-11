(ns aoc2025.day11
  (:require [aoc2025.core :as aoc]))

(defn parse [data]
  (->> (aoc/parse-lines data :words {:word-sep #":? +"})
       (map (fn [[from & tos]] [from tos]))
       (into {})))

(def count-paths
  (memoize
   (fn [graph from to]
     (if (= from to)
       1
       (transduce (map #(count-paths graph % to)) + 0 (graph from))))))

(defn part1 [data] (count-paths (parse data) "you" "out"))

(defn part2 [data]
  (let [paths (partial count-paths (parse data))]
    (+ (* (paths "svr" "dac") (paths "dac" "fft") (paths "fft" "out"))
       (* (paths "svr" "fft") (paths "fft" "dac") (paths "dac" "out")))))

(assert (= 636             (part1 (aoc/day 11))))
(assert (= 509312913844956 (part2 (aoc/day 11)))) ;=> 3ms
