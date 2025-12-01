(ns aoc2025.day1
  (:require [aoc2025.core :as aoc]
            [clojure.string :as str]))

(defn parse [data]
  (for [line (str/split-lines data)
        :let [[dir & nums] line
              distance (parse-long (apply str nums))]]
    [(if (= dir \L) :left :right) distance]))

(defn rotate [position direction distance]
  (mod ((if (= direction :left) - +) position distance) 100))

(defn part1 [data]
  (let [rotations (parse data)]
    (->> rotations
         (reductions (fn [pos [dir dist]]
                      (rotate pos dir dist))
                    50)
         rest  ; skip initial position
         (filter zero?)
         count)))

(defn count-zeros-during-rotation [start direction distance]
  (when (pos? distance)
    (let [first-hit (if (zero? start) 100
                        (if (= direction :left) start (- 100 start)))]
      (when (>= distance first-hit)
        (inc (quot (- distance first-hit) 100))))))

(defn part2 [data]
  (first
   (reduce (fn [[total pos] [dir dist]]
             (let [zeros (or (count-zeros-during-rotation pos dir dist) 0)]
               [(+ total zeros) (rotate pos dir dist)]))
           [0 50]
           (parse data))))

(assert (= 1129 (part1 (aoc/day 1))))
(assert (= 6638 (part2 (aoc/day 1))))
