(ns aoc2025.day4
  (:require [aoc2025.core :as aoc]))

(def directions
  (for [dy [-1 0 1] dx [-1 0 1] :when (not (and (zero? dy) (zero? dx)))]
    [dy dx]))

(defn parse [data] (aoc/index-2d (aoc/parse-lines data :chars)))

(defn- count-adjacent-rolls [grid [y x]]
  (->> directions
       (map (fn [[dy dx]] [(+ y dy) (+ x dx)]))
       (filter #(= \@ (grid %)))
       count))

(defn- accessible? [grid pos] (< (count-adjacent-rolls grid pos) 4))

(defn- find-rolls [grid]
  (keep (fn [[pos ch]] (when (= ch \@) pos)) grid))

(defn- remove-accessible [{:keys [grid]}]
  (let [accessible (->> grid
                        find-rolls
                        (filter (partial accessible? grid)))]
    {:grid (reduce #(assoc %1 %2 \.) grid accessible)
     :count (count accessible)}))

(defn part1 [data]
  (let [grid (parse data)]
    (->> (parse data)
         find-rolls
         (filter #(accessible? grid %))
         count)))

(defn part2 [data]
  (->> {:grid (parse data)}
       (iterate remove-accessible)
       rest
       (map :count)
       (take-while pos?)
       (reduce +)))

(assert (= 1409 (part1 (aoc/day 4))))
(assert (= 8366 (part2 (aoc/day 4))))
