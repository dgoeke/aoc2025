(ns aoc2025.day2
  (:require [aoc2025.core :as aoc]
            [clojure.string :as str]))

(defn parse [data]
  (->> (str/split (str/replace data #"\s+" "") #",")
       (map #(aoc/integers % :negative? false))))

(defn generate-patterns [pattern-len reps start end]
  (let [min-pattern (long (Math/pow 10 (dec pattern-len)))
        max-pattern (dec (long (Math/pow 10 pattern-len)))]
    (for [pattern (range min-pattern (inc max-pattern))
          :let [s (str pattern)
                repeated (str/join (repeat reps s))
                n (parse-long repeated)]
          :when (<= start n end)]
      n)))

(defn invalid-ids-part1 [start end]
  (let [start-len (count (str start))
        end-len (count (str end))]
    (for [total-len (range start-len (inc end-len))
          :when (even? total-len)
          :let [pattern-len (quot total-len 2)]
          n (generate-patterns pattern-len 2 start end)]
      n)))

(defn invalid-ids-part2 [start end]
  (let [start-len (count (str start))
        end-len (count (str end))]
    (distinct
     (for [total-len (range start-len (inc end-len))
           pattern-len (range 1 (inc total-len))
           :let [reps (quot total-len pattern-len)]
           :when (and (zero? (rem total-len pattern-len))
                      (>= reps 2))
           n (generate-patterns pattern-len reps start end)]
       n))))

(defn part1 [data]
  (->> (parse data)
       (mapcat (partial apply invalid-ids-part1))
       (reduce +)))

(defn part2 [data]
  (->> (parse data)
       (mapcat (partial apply invalid-ids-part2))
       (reduce +)))

(time (assert (= 55916882972 (part1 (aoc/day 2)))))
(time (assert (= 76169125915 (part2 (aoc/day 2))))) ; 33ms


