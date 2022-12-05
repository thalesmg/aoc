(ns day02.sol
  (:require
   [clojure.string :as str]))

(def raw-input
  (slurp "input"))

(defn parse-opponent
  [x]
  (case x
    "A" :rock
    "B" :paper
    "C" :scissors))

(defn parse-move
  [x]
  (case x
    "X" :rock
    "Y" :paper
    "Z" :scissors))

(def points-per-move
  {:rock 1
   :paper 2
   :scissors 3})

(def opposite
  {:won :lost
   :lost :won
   :draw :draw})

(defn outcome
  [opponent me]
  (if (= opponent me)
    :draw
    (case [opponent me]
      [:rock :paper] :won
      [:rock :scissors] :lost
      [:paper :scissors] :won
      (opposite (outcome me opponent)))))

(def points-per-outcome
  {:draw 3
   :won 6
   :lost 0})

(def parsed-moves
  (->> raw-input
       str/split-lines
       (map (fn [line]
              (let [[op me] (str/split line #" ")]
                [(parse-opponent op) (parse-move me)])))))

(def part1
  (->> parsed-moves
       (map (fn [[op me]]
              (+ (points-per-move me) (points-per-outcome (outcome op me)))))
       (apply +)))

(defn parse-desired
  [x]
  (case x
    "X" :lost
    "Y" :draw
    "Z" :won))

(def wins-over
  {:rock :scissors
   :scissors :paper
   :paper :rock})

(def loses-for
  (->> wins-over
       (map (fn [[x y]] [y x]))
       (into {})))

(defn desired-to-move
  [op desired]
  (case desired
    :lost (wins-over op)
    :won (loses-for op)
    op))

(def parsed-moves'
  (->> raw-input
       str/split-lines
       (map (fn [line]
              (let [[op me] (str/split line #" ")]
                [(parse-opponent op) (parse-desired me)])))))

(def part2
  (->> parsed-moves'
       (map (fn [[op desired]]
              (+ (points-per-move (desired-to-move op desired)) (points-per-outcome desired))))
       (apply +)))
