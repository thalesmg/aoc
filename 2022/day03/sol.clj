(ns day03.sol
  (:require
   [clojure.set :as set]
   [clojure.string :as str]))

(def raw-input
  (slurp "input"))

(def compartments
  (->> raw-input
       str/split-lines
       (map #(split-at (/ (count %) 2) %))))

(def common-items
  (->> compartments
       (map (fn [[l r]]
              (let [l (set l)
                    r (set r)]
                (first (set/intersection l r)))))))

(def item->prio
  (let [lowers (zipmap (map char (range 97 (+ 97 27)))
                       (range 1 27))
        uppers (zipmap (map char (range 65 (+ 65 27)))
                       (range 27 53))]
    (merge lowers uppers)))

(def part1
  (->> common-items
       (map item->prio)
       (apply +)))

(def common-items3
  (->> raw-input
       str/split-lines
       (partition 3)
       (map #(->> %
                  (map set)
                  (apply set/intersection)
                  first))))

(def part2
  (->> common-items3
       (map item->prio)
       (apply +)))
