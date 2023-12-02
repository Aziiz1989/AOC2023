(ns aziz.aoc2023
  (:require [clojure.string :as s]))

(defn get-data [file-name]
  (->> (slurp file-name)
       (s/split-lines)))

(defn get-nums [s]
  (map #(re-seq #"\d+" %) s))

(def part-1-test ["1abc2"
                  "pqr3stu8vwx"
                  "a1b2c3d4e5f"
                  "treb7uchet"])

(defn part-1 [s]
  (->> s                ;; (take 5)
       (get-nums)
       (map #(s/join "" %))
       (map #(str (first %) (last %)))
       (map #(Integer/parseInt %))
       (apply +)))

(comment
  (part-1 (get-data "input.txt")))

(def char-digits-map {"one" "1"
                      "two" "2"
                      "three" "3"
                      "four" "4"
                      "five" "5"
                      "six" "6"
                      "seven" "7"
                      "eight" "8"
                      "nine" "9"})

(defn char->digits [s]
  (map #(s/replace % #"one|two|three|four|five|six|seven|eight|nine" char-digits-map) s))

(defn get-all-chars [s]
  (map #(re-seq #"(?=(one|two|three|four|five|six|seven|eight|nine|[0-9]))" %) s))

(def t2 ["two1nine"
         "eightwothree"
         "abcone2threexyz"
         "xtwone3four"
         "4nineeightseven2"
         "zoneight234"
         "7pqrstsixteen"])

(defn part-2 [s]
  (->> s
       (get-all-chars)
       (map flatten)
       (map #(s/join "" %))
       (char->digits)
       (map #(str (first %) (last %)))
       (map #(Integer/parseInt %))
       (apply +)))

(part-2 (get-data "input.txt"))




