(ns advent-of-code.day-01
  (:require [aocd.core :as aoc]
            [cuerdas.core :as str]))

(def input
  (-> (aoc/input 2021 1)
      (str/split "\n")
      (->>
       (mapv read-string))))

(defn count-increasing?
  [coll]
  (count (filter (partial apply <) (partition 2 1 coll))))

(defn sliding-window
  ([coll] (sliding-window 2 coll))
  ([n coll] (partition n 1 coll)))

(defn part-1
  "Day 01 Part 1"
  []
  (count-increasing? input))

(defn part-2
  "Day 01 Part 2"
  []
  (->> input
       (sliding-window 3)
       (map (partial apply +))
       (count-increasing?)))
