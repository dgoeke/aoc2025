(ns aoc2025.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:gen-class))

(defn- load-resource [filename]
  (some->> filename io/resource slurp))

(defn day
  "Load input data from resources directory. Returns `nil`
   if file does not exist, or a string if it does.
    (day 1)                => day1.txt
    (day 1 :sample)        => day1-sample.txt
    (day 2 :sample :part2) => day2-sample-part2.txt"
  [n & args]
  (load-resource
   (str (str/join "-" (cons (str "day" n)
                            (map name args)))
        ".txt")))

(defn integers
  [s & {:keys [negative?]
        :or {negative? true}}]
  (mapv parse-long
        (re-seq (if negative? #"-?\d+" #"\d+") s)))

(defn string->digits [s]
  (->> (str/split s #"")
       (map parse-long)
       (filterv some?)))

(defn parse-line [line & [parse-fn word-sep]]
  (let [f (case parse-fn
            :int    parse-long
            :ints   integers
            :digits string->digits
            :chars  vec
            :words  #(str/split % (or word-sep #" "))
            nil     identity
            parse-fn)]
    (f line)))

(defn parse-lines
  [s & [parse-fn {:keys [word-sep nl-sep]}]]
  (mapv #(parse-line % parse-fn word-sep)
        (str/split s (or nl-sep #"\n"))))

(defn transpose [matrix]
  (apply mapv vector matrix))

(defn parse-paragraphs
  [input & [parse-fn word-sep]]
  (mapv #(parse-lines % parse-fn {:word-sep word-sep})
        (parse-lines input nil {:nl-sep #"\n\n"})))

(defn index-2d [lines]
  (into {} (apply concat (map-indexed (fn [y line] (map-indexed (fn [x ch] [[y x] ch]) line)) lines))))
