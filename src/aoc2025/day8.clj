(ns aoc2025.day8
  (:require [aoc2025.core :as aoc]))

(defn parse [data]
  (aoc/parse-lines data :ints))

(defn distance [p1 p2]
  (->> (map - p1 p2) (map #(* % %)) (reduce +) Math/sqrt))

;; union-find with plain vector
(defn find-root [parent i]
  (if (= i (parent i)) i (recur parent (parent i))))

(defn uf-union [parent a b]
  (let [ra (find-root parent a)
        rb (find-root parent b)]
    (if (= ra rb) parent (assoc parent rb ra))))

(defn connected? [parent a b]
  (= (find-root parent a) (find-root parent b)))

(defn num-circuits [parent]
  (count (distinct (map #(find-root parent %) (range (count parent))))))

(defn sorted-pairs [coords]
  (let [n (count coords)]
    (->> (for [i (range n) j (range (inc i) n)]
           [i j (distance (coords i) (coords j))])
         (sort-by last))))

(defn circuit-sizes [parent]
  (->> (range (count parent))
       (group-by #(find-root parent %))
       vals
       (map count)
       (sort >)))

(defn connect [coords limit]
  (reduce
   (fn [{:keys [parent attempts] :as state} [i j]]
     (if (>= attempts limit)
       (reduced state)
       (if (connected? parent i j)
         (update state :attempts inc)
         (-> state
             (update :attempts inc)
             (update :parent uf-union i j)))))
   {:parent (vec (range (count coords))) :attempts 0}
   (sorted-pairs coords)))

(defn connect-until-one [coords]
  (reduce
   (fn [{:keys [parent] :as state} [i j]]
     (if (connected? parent i j)
       state
       (let [new-parent (uf-union parent i j)]
         (if (= 1 (num-circuits new-parent))
           (reduced {:parent new-parent :last [i j]})
           (assoc state :parent new-parent)))))
   {:parent (vec (range (count coords)))}
   (sorted-pairs coords)))

(defn part1 [data]
  (let [coords           (parse data)
        {:keys [parent]} (connect coords 1000)]
    (apply * (take 3 (circuit-sizes parent)))))

(defn part2 [data]
  (let [coords         (parse data)
        {:keys [last]} (connect-until-one coords)
        [i j]          last]
    (* (first (coords i)) (first (coords j)))))

(assert (= 129564 (part1 (aoc/day 8))))
(assert (= 42047840 (part2 (aoc/day 8))))
