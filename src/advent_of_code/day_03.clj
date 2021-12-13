(ns advent-of-code.day-03
  (:require [aocd.core :as aoc]
            [cuerdas.core :as str]
            [clojure.core.matrix :as mat]))

(defn parse
  [s]
  (-> s
      (str/split)
      (->>
       (mapv (comp
              (partial mapv read-string)
              #(str/split %1 ""))))))

(def input
  (parse (aoc/input 2021 3)))

(def example
  (parse "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010"))

(defn prp
  [x]
  (prn x)
  x)

(defn bitwise-reduce
  [f mm]
  (->> mm mat/transpose f mat/transpose))

(defn bit-reducer
  [v]
  )

(defn mcbs
  [mm]
  (bitwise-reduce 
   (fn [m] (mapv #(if (<= (/ (count %) 2) (apply + %)) 1 0) m))
   mm))

(defn lcbs
  [mm]
  (bitwise-reduce
   (fn [m] (mapv #(if (>= (/ (count %) 2) (apply + %)) 1 0) m))
   mm))

(defn γ*ε
  [mm]
  (->> mm
       ((juxt mcbs lcbs))
       (mapv (comp #(Integer/parseInt % 2) str/join))
       (#(apply * %))))

(defn O₂
  [mm]
  (->> mm
       mcbs
       (map-indexed (fn [n bit] (filter #(= (nth % n) bit) mm)))))

(comment
  (mcbs example)
  (->> example mcbs (map-indexed vector))
  (O₂ example)
  (γ*ε example))

(defn part-1
  "Day 03 Part 1"
  []
  (γ*ε input))

(defn part-2
  "Day 03 Part 2"
  [])
