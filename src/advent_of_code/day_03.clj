(ns advent-of-code.day-03
  (:require [aocd.core :as aoc]
            [cuerdas.core :as str]
            [clojure.core.matrix :as mat]
            #_[clojure.core.vecotrz :as vec]))

(mat/set-current-implementation :vectorz)

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

(defn joins 
  [sep & parts]
  (str/join sep (flatten parts)))

(defn mat?
  [mm]
  (and (seqable? mm)
       (seq mm)
       #_(vector? mm)
       (< 1 (count (mat/shape mm)))))

(defn printmatln
  ([mm]
   (if-not (mat? mm)
     (println mm)
     (do
       (println (joins " " "⎡" (conj (first mm) "⎤")))
       (doall (map #(println (joins " " "⎢" (conj % "⎥"))) (butlast (next mm))))
       (println (joins " " "⎣" (conj (last mm) "⎦"))))))
  ([mm & nn]
   (let [{nnn false
          mmm true} (group-by mat? (cons mm nn))]
     (apply println nnn)
     (doall (map printmatln mmm)))))


(def read-bin (comp #(Integer/parseInt % 2) str/join))

(defn bitwise-mapv
  [f mm]
  (->> mm mat/transpose f mat/transpose))

(defn mcbs
  [mm]
  (bitwise-mapv
   (fn [m] (mapv #(if (<= (/ (count %) 2) (apply + %)) 1 0) m))
   mm))

(defn lcbs
  [mm]
  (bitwise-mapv
   (fn [m] (mapv #(if (<= (/ (count %) 2) (apply + %)) 0 1) m))
   mm))

(defn γ*ε
  [mm]
  (->> mm
       ((juxt mcbs lcbs))
       (mapv read-bin)
       (#(apply * %))))

(defn extract-report
  [mask-fn mm]
  (printmatln (mask-fn mm) mm)
  (letfn
   [(filt
      [n m]
      (let [nbit (-> m mask-fn (nth n))
            grouped
            (group-by #(= nbit (nth % n)) m)]
        (printmatln true (grouped true) false (grouped false))
        (if (seq (grouped true)) (grouped true) (grouped false))))]
    (loop [n 0
           report (filt n mm)]
      (printmatln n (mask-fn mm) report)
      (if (or (>= n  (-> report mat/shape second))
              (<= (-> report set count) 1))
        (-> report first read-bin)
        (recur (inc n)
               (filt n report))))))
              

(defn O₂gen
  [mm]
  (extract-report mcbs mm))

(defn CO₂scrub
  [mm]
  (extract-report lcbs mm))

(defn life-support
  [mm]
  (->> mm
       ((juxt O₂gen CO₂scrub))
       (apply *)))

(comment
  (vector? example)
  (mat? example)
  (< 1 (count (mat/shape example)))
  (printmatln example)
  (->> example mcbs (map-indexed vector))
  (O₂gen example)
  (CO₂scrub example)
  (γ*ε example)
  (life-support example))

(defn part-1
  "Day 03 Part 1"
  []
  (γ*ε input))

(defn part-2
  "Day 03 Part 2"
  []
  (life-support input))
(comment
  (part-1)
  (part-2))