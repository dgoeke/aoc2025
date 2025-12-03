(ns aoc2025.day3
  (:require [aoc2025.core :as aoc]))

(defn parse [data] (aoc/parse-lines data :chars))

(defn- pick-digits [bank n]
  (lazy-seq
   (when (pos? n)
     (let [window (subvec bank 0 (inc (- (count bank) n)))
           max-c (reduce #(if (neg? (compare %1 %2)) %2 %1) window)
           idx (.indexOf window max-c)]
       (cons max-c (pick-digits (subvec bank (inc idx)) (dec n)))))))

(defn max-subsequence [bank n]
  (parse-long (apply str (pick-digits bank n))))

(defn part1 [data]
  (transduce (map #(max-subsequence % 2)) + (parse data)))

(defn part2 [data]
  (transduce (map #(max-subsequence % 12)) + (parse data)))

(assert (= 17263           (part1 (aoc/day 3))))
(assert (= 170731717900423 (part2 (aoc/day 3))))
