(ns aoc2025.day6
  (:require [aoc2025.core :as aoc]
            [clojure.string :as str]))

(def ops {\+ + \* *})

;; part 1: token-based (split by whitespace, transpose)
(defn parse1 [data]
  (->> (str/split-lines data)
       (map #(remove str/blank? (str/split % #"\s+")))
       (map vec)
       aoc/transpose
       (map (fn [col]
              {:nums (map parse-long (butlast col))
               :op   (first (last col))}))))

;; part 2: character-based, right-to-left, vertical numbers
(defn pad-lines [lines]
  (let [w (apply max (map count lines))]
    (map #(format (str "%-" w "s") %) lines)))

(defn parse-col [col]
  (let [digits (remove #{\space} (butlast col))]
    {:num (when (seq digits) (parse-long (apply str digits)))
     :op  (when (ops (last col)) (last col))}))

(defn parse2 [data]
  (->> (str/split-lines data)
       pad-lines
       aoc/transpose
       (partition-by #(every? #{\space} %))
       (remove #(every? #{\space} (first %)))
       (map (fn [grp]
              (let [parsed (map parse-col (reverse grp))]
                {:nums (keep :num parsed)
                 :op   (some :op parsed)})))))

(defn part1 [data]
  (transduce (map #(reduce (ops (:op %)) (:nums %))) + (parse1 data)))

(defn part2 [data]
  (transduce (map #(reduce (ops (:op %)) (:nums %))) + (parse2 data)))

(assert (= 5322004718681 (part1 (aoc/day 6))))
(assert (= 9876636978528 (part2 (aoc/day 6))))
