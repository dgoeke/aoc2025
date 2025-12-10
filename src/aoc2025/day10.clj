(ns aoc2025.day10
  (:require [aoc2025.core :as aoc]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-machine [line]
  (let [[_ target-str]  (re-find #"\[([.#]+)\]" line)
        target          (set (keep-indexed #(when (= %2 \#) %1) target-str))
        button-strs     (map second (re-seq #"\(([0-9,]+)\)" line))
        buttons         (mapv #(set (map parse-long (str/split % #","))) button-strs)
        [_ joltage-str] (re-find #"\{([0-9,]+)\}" line)
        joltages        (mapv parse-long (str/split joltage-str #","))]
    {:target   target
     :buttons  buttons
     :joltages joltages}))

(defn parse [data]
  (map parse-machine (str/split-lines data)))

(defn toggle-lights [state lights]
  (set/difference (set/union state lights)
                  (set/intersection state lights)))

(defn lights-for-mask [buttons mask]
  (->> (range (count buttons))
       (filter #(bit-test mask %))
       (map buttons)
       (reduce toggle-lights #{})))

(defn min-presses [{:keys [target buttons]}]
  (let [n (count buttons)]
    (->> (range (bit-shift-left 1 n))
         (keep #(when (= target (lights-for-mask buttons %))
                  (Long/bitCount %)))
         (apply min))))

(defn part1 [data]
  (->> (parse data)
       (map min-presses)
       (reduce +)))

(defn gaussian-eliminate [aug-matrix]
  (let [m (count aug-matrix)
        n (dec (count (first aug-matrix)))]
    (loop [mat aug-matrix, row 0, col 0, pivots []]
      (if (or (>= row m) (>= col n))
        [mat pivots]
        (let [pivot-row (->> (range row m)
                             (remove #(zero? (get-in mat [% col])))
                             first)]
          (if-not pivot-row
            (recur mat row (inc col) pivots)
            (let [mat       (if (= pivot-row row) mat
                                (assoc mat row (mat pivot-row) pivot-row (mat row)))
                  pivot-val (get-in mat [row col])
                  mat       (update mat row (fn [r] (mapv #(/ % pivot-val) r)))
                  mat       (reduce (fn [m i]
                                      (if (= i row) m
                                          (let [factor (get-in m [i col])]
                                            (update m i (fn [r] (mapv #(- %1 (* factor %2)) r (m row)))))))
                                    mat (range m))]
              (recur mat (inc row) (inc col) (conj pivots col)))))))))

(defn solve-from-free-vars [rref pivots n free-vals]
  (let [pivot-set (set pivots)
        free-vars (vec (remove pivot-set (range n)))
        sol       (reduce (fn [s [fv-idx fv-val]]
                            (assoc s (free-vars fv-idx) fv-val))
                          (vec (repeat n 0))
                          (map-indexed vector free-vals))]
    (reduce (fn [s [row pivot-col]]
              (let [rhs          (last (rref row))
                    free-contrib (reduce + (map (fn [fv]
                                                  (* (get-in rref [row fv]) (s fv)))
                                                free-vars))]
                (assoc s pivot-col (- rhs free-contrib))))
            sol
            (map-indexed vector pivots))))

(def non-neg? (every-pred integer? (complement neg?)))

(defn valid-solution? [sol] (every? non-neg? sol))

(defn partial-pivot-value [rref coeff v idx pi]
  (reduce (fn [a k] (- a (* (coeff pi k) (v k))))
          (last (rref pi))
          (range idx)))

(defn can-become-non-neg? [rref coeff num-free v idx pi]
  (let [pv (partial-pivot-value rref coeff v idx pi)]
    (or (>= pv 0)
        (some #(neg? (coeff pi %)) (range idx num-free)))))

(defn infeasible? [rref num-free coeff num-pivots idx v]
  (not-every? #(can-become-non-neg? rref coeff num-free v idx %) (range num-pivots)))

(defn solve-var [rref num-free coeff fv->idx pivot->row v j]
  (if-let [fv-idx (fv->idx j)]
    (v fv-idx)
    (let [pi (pivot->row j)]
      (reduce (fn [s k] (- s (* (coeff pi k) (v k))))
              (last (rref pi))
              (range num-free)))))

(defn branch-and-bound [rref pivots n free-vars num-free coeff max-j sum-j]
  (let [fv->idx    (zipmap free-vars (range))
        pivot->row (zipmap pivots (range))
        num-pivots (count pivots)]
    (loop [best  Long/MAX_VALUE
           stack (list [0 (vec (repeat num-free 0)) 0])]
      (if (empty? stack)
        (when (< best Long/MAX_VALUE) best)
        (let [[i v s]    (first stack)
              rest-stack (rest stack)]
          (cond
            (or (>= s best) (> s sum-j)
                (infeasible? rref num-free coeff num-pivots i v))
            (recur best rest-stack)

            (= i num-free)
            (let [sol (mapv #(solve-var rref num-free coeff fv->idx pivot->row v %) (range n))]
              (if (valid-solution? sol)
                (recur (min best (reduce + (map long sol))) rest-stack)
                (recur best rest-stack)))

            :else
            (let [max-val  (min max-j (- sum-j s))
                  children (for [x (range (inc max-val))]
                             [(inc i) (assoc v i x) (+ s x)])]
              (recur best (concat children rest-stack)))))))))

(defn minimize-button-presses [rref pivots n joltages]
  (let [pivot-set (set pivots)
        free-vars (vec (remove pivot-set (range n)))
        num-free  (count free-vars)
        max-j     (apply max joltages)
        sum-j     (reduce + joltages)
        coeff     (fn [pi k] (get-in rref [pi (free-vars k)]))]
    (if (zero? num-free)
      (let [sol (solve-from-free-vars rref pivots n [])]
        (when (valid-solution? sol)
          (reduce + (map long sol))))
      (branch-and-bound rref pivots n free-vars num-free coeff max-j sum-j))))

(defn inconsistent-row? [row]
  (and (every? zero? (butlast row))
       (not (zero? (last row)))))

(defn solve-system [{:keys [buttons joltages]}]
  (let [n             (count buttons)
        m             (count joltages)
        aug           (vec (for [j (range m)]
                             (conj (mapv #(if (contains? % j) 1 0) buttons)
                                   (joltages j))))
        [rref pivots] (gaussian-eliminate aug)]
    (when-not (some inconsistent-row? (drop (count pivots) rref))
      (minimize-button-presses rref pivots n joltages))))

(defn part2 [data]
  (->> (parse data)
       (map solve-system)
       (reduce +)))

(time (assert (= 436 (part1 (aoc/day 10)))))
(time (assert (= 14999 (part2 (aoc/day 10))))) ; => 940ms
