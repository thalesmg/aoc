(ns day01.sol
  (:require
   [clojure.string :as str]))

(def input
  (slurp "input"))

(def part1
  (-> input
      (str/split #"\n\n")
      (->>
       (map (fn [elf]
              (->> elf
                   str/split-lines
                   (map #(Integer/parseInt %))
                   (apply +))))
       (apply max))))

(def part2
  (-> input
      (str/split #"\n\n")
      (->>
       (map (fn [elf]
              (->> elf
                   str/split-lines
                   (map #(Integer/parseInt %))
                   (apply +))))
       sort
       reverse
       (take 3)
       (apply +))))
