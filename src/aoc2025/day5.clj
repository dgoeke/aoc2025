(ns aoc2025.day5
  (:require [aoc2025.core :as aoc]
            [clojure.string :as str]))

(defn parse [data]
  (let [[ranges-lines ids-lines] (aoc/parse-paragraphs data)
        ranges                   (map #(aoc/integers % :negative? false) ranges-lines)
        ids                      (map parse-long ids-lines)]
    {:ranges ranges
     :ids    ids}))

(defn fresh? [id ranges]
  (some (fn [[start end]]
          (<= start id end))
        ranges))

(defn part1 [data]
  (let [{:keys [ranges ids]} (parse data)]
    (count (filter #(fresh? % ranges) ids))))

(defn merge-ranges [ranges]
  (let [sorted (sort-by first ranges)]
    (reduce (fn [merged [start end]]
              (if (empty? merged)
                [[start end]]
                (let [[prev-start prev-end] (peek merged)]
                  (if (<= start (inc prev-end))
                    (conj (pop merged) [prev-start (max prev-end end)]) ; overlapping/adjacent
                    (conj merged [start end])))))                       ; not overlapping
            []
            sorted)))

(defn count-ids-in-ranges [ranges]
  (transduce (map (fn [[start end]] (inc (- end start))))
             +
             (merge-ranges ranges)))

(defn part2 [data]
  (let [{:keys [ranges]} (parse data)]
    (count-ids-in-ranges ranges)))

(assert (= 513 (part1 (aoc/day 5))))
(assert (= 339668510830757 (part2 (aoc/day 5))))
