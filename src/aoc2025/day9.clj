(ns aoc2025.day9
  (:require [aoc2025.core :as aoc]))

(defn parse [data] (aoc/parse-lines data :ints))

(defn rect-area [[x1 y1] [x2 y2]]
  (* (inc (abs (- x2 x1)))
     (inc (abs (- y2 y1)))))

(defn max-rect-area [points pred]
  (reduce max 0
          (for [p1    points, p2 points
                :when (and (not= p1 p2) (pred p1 p2))]
            (rect-area p1 p2))))

(defn part1 [data] (max-rect-area (parse data) (constantly true)))

(defn polygon-edges [points]
  (partition 2 1 (concat points [(first points)])))

(defn ray-crosses-edge? [[px py] [[x1 y1] [x2 y2]]]
  (and (not= y1 y2)
       (or (< y1 py y2) (< y2 py y1))
       (> x1 px)))

(defn point-in-polygon? [edges point]
  (->> edges
       (filter #(ray-crosses-edge? point %))
       count
       odd?))

(defn compress-coords [coords]
  (let [sorted (vec (sort (distinct coords)))]
    {:coords sorted
     :->idx  (zipmap sorted (range))}))

(defn build-inside-grid [xs ys inside?]
  (let [midpoint (fn [v i] (/ (+ (v i) (v (inc i))) 2))]
    (vec (for [i (range (dec (count xs)))]
           (vec (for [j (range (dec (count ys)))]
                  (if (inside? [(midpoint xs i) (midpoint ys j)]) 1 0)))))))

(defn build-prefix-sum [grid]
  (let [cols (inc (count (first grid)))]
    (->> grid
         (mapv #(vec (reductions + 0 %)))
         (reductions #(mapv + %1 %2) (vec (repeat cols 0)))
         vec)))

(defn query-rect-sum [prefix i1 j1 i2 j2]
  (+ (get-in prefix [i2 j2] 0)
     (- (get-in prefix [i1 j2] 0))
     (- (get-in prefix [i2 j1] 0))
     (get-in prefix [i1 j1] 0)))

(defn rect-validator-fn [points]
  (let [edges                      (polygon-edges points)
        {xs :coords x->idx :->idx} (compress-coords (map first points))
        {ys :coords y->idx :->idx} (compress-coords (map second points))
        inside?                    (partial point-in-polygon? edges)
        grid                       (build-inside-grid xs ys inside?)
        prefix                     (build-prefix-sum grid)]
    (fn [[x1 y1] [x2 y2]]
      (let [i1        (x->idx (min x1 x2)), i2 (x->idx (max x1 x2))
            j1        (y->idx (min y1 y2)), j2 (y->idx (max y1 y2))
            num-cells (* (- i2 i1) (- j2 j1))]
        (= num-cells (query-rect-sum prefix i1 j1 i2 j2))))))

(defn part2 [data]
  (let [points (parse data)]
    (max-rect-area points (rect-validator-fn points))))

(assert (= 4750176210 (part1 (aoc/day 9))))
(assert (= 1574684850 (part2 (aoc/day 9))))
