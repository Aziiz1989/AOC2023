(ns aziz.day02
  (:require
   [clojure.string :as s]))

(def test-data
  "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
   Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
   Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
   Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
   Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

(def test-data-list
  (map s/trim (s/split-lines t)))

(defn get-data [file-name]
  (->> (slurp file-name)
       (s/split-lines)))

(def base-map {"blue" []
               "green" []
               "red" []})

(defn ->map [s base-map]
  ;; (prn s)
  (loop [base-map base-map
         colors s]
    (if (empty? colors)
      base-map
      (recur (update-in base-map [(second (first colors))] conj (Integer/parseInt (first (first colors))))
             (rest colors)))))

(defn txt->map [i]
  (let [tokens (s/split i #":|;|,")
        game-num (Integer/parseInt (re-find #"\d+" (first tokens)))
        trim-ws (map s/triml tokens)
        splits (map #(s/split % #" ") (rest trim-ws))
        to_map (->map splits base-map)]
    {game-num to_map}))

(defn is_game_possible [col conditions]
  (let [results (first (vals col))
        ;; _ (prn (get results "green") (get results "blue") (get results "red"))
        is_blue? (every? #(<= % (get conditions "blue")) (get results "blue"))
        is_red? (every? #(<= % (get conditions "red")) (get results "red"))
        is_green? (every? #(<= % (get conditions "green")) (get results "green"))
        ;; _ (prn is_green? is_blue? is_red?)
        is_possible? (every? true? [is_red? is_blue? is_green?])]
    (if is_possible? (first (keys col)) 0)))

(defn power-set [col]
  (let [results (first (vals col))
        max (map #(apply max (val %)) results)
        power-set (apply * max)]
    power-set))

;;part-1 solution
(apply + (map #(is_game_possible % {"blue" 14 "green" 13 "red" 12}) (map txt->map (get-data "day02_input.txt"))))

;;part-1 solution 
(apply + (map power-set (map txt->map (get-data "day02_input.txt"))))



