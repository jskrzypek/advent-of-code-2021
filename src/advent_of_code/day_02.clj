(ns advent-of-code.day-02
  (:require [aocd.core :as aoc]
            [cuerdas.core :as str]))

(defn parse
  [s]
  (-> s
      (str/split)
      (->>
       (partition 2)
       (mapv (partial map #(%1 %2) [keyword read-string])))))

(def input
  (parse (aoc/input 2021 2)))

(def example
  (parse "forward 5
down 5
forward 8
up 3
down 8
forward 2"))

(defn move
  [[x y] [dir val]]
  (case dir
    :forward [(+ x val) y]
    :down [x (+ y val)]
    :up [x (- y val)]))

(defn move-2
  [[x y aim] [dir val]]
  (case dir
    :forward [(+ x val) (+ y (* aim val)) aim]
    :down [x y (+ aim val)]
    :up [x y (- aim val)]))

(defn part-1
  "Day 02 Part 1"
  []
  (->> input
       (reduce move [0 0])
       (apply *)))

(comment 
  (reduce move [0 0] example)
  (reduce move-2 [0 0 0] example))

(defn part-2
  "Day 02 Part 2"
  []
  (->> input
       (reduce move-2 [0 0 0])
       (take 2)
       (apply *)))
