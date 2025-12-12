(ns aoc2025.day12
  (:require [aoc2025.core :as aoc]))

(defn parse [data]
  (let [paragraphs (aoc/parse-paragraphs data)
        shapes     (butlast paragraphs)
        regions    (last paragraphs)]
    {:areas   (mapv #(count (filter #{\#} (apply str %))) shapes)
     :regions (mapv aoc/integers regions)}))

(defn fits? [areas [w h & counts]]
  (<= (reduce + (map * areas counts))
      (* w h)))

(defn part1 [data]
  (let [{:keys [areas regions]} (parse data)]
    (count (filter #(fits? areas %) regions))))

(assert (= 526 (part1 (aoc/day 12))))
