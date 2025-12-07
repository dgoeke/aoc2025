(ns aoc2025.day7
  (:require [aoc2025.core :as aoc]))

(defn parse [data]
  (let [grid (-> (aoc/parse-lines data :chars)
                 aoc/index-2d)
        start (first (for [[pos ch] grid :when (= ch \S)] pos))]
    {:grid grid, :start start}))

(defn next-pos [[y x] dir]
  (case dir
    :down  [(inc y) x]
    :left  [y (dec x)]
    :right [y (inc x)]))

(defn step [grid count-on acc [[pos dir] cnt]]
  (let [ch (get grid pos)]
    (cond
      (nil? ch)
      (cond-> acc
        (= count-on :exit) (update :result + cnt))

      (= ch \^)
      (let [left-pos      (next-pos pos :left)
            right-pos     (next-pos pos :right)
            already-seen? ((:seen acc) pos)]
        (cond-> acc
          (and (= count-on :split) (not already-seen?)) (update :result inc)
          true (update :seen conj pos)
          true (update-in [:next-beams [left-pos :down]] (fnil + 0) cnt)
          true (update-in [:next-beams [right-pos :down]] (fnil + 0) cnt)))

      :else
      (update-in acc [:next-beams [(next-pos pos dir) dir]] (fnil + 0) cnt))))

(defn simulate
  [grid start count-on]
  (loop [beams          {[start :down] 1}
         total          0
         seen-splitters #{}]
    (if (empty? beams)
      total
      (let [{:keys [next-beams result seen]}
            (reduce (partial step grid count-on)
                    {:next-beams {} :result 0 :seen seen-splitters}
                    beams)]
        (recur next-beams (+ total result) seen)))))

(defn part1 [data]
  (let [{:keys [grid start]} (parse data)]
    (simulate grid start :split)))

(defn part2 [data]
  (let [{:keys [grid start]} (parse data)]
    (simulate grid start :exit)))

(assert (= 1633 (part1 (aoc/day 7))))
(assert (= 34339203133559 (part2 (aoc/day 7))))
