(ns alt
  (:require [clojure.string :as string]))


(def word-set (set (map string/upper-case (string/split-lines (slurp "words.txt")))))

(def puzzle [[4 \P] [0 2] [1 \N] [3 4] [0 \F] [1 2] [3 \C] [0 4] [2 \A]])

;; Convert input-data into 2 representations of the board (a 2d vector of rows)
;; - a row-outline: vector of the row vectors
;; - a represention: for now a tranformation of the base representation into a full rep
;; - A link-outline: vector of the link vectors
;;
;; Gameloop
;; - print the full representation
;; - take user input
;; - validate new representation
;; - if error: print feedback, start loop with last representation
;; - if good:
;; - check for win
;; - if no, start loop again
;; - if yes, print congratz and exit game
;;
;; Validation
;; - Check rows of representation against row-outline
;; - check pairs of rows of representation against link-outline


(defn- represent [letters links words]
  (let [full-links (map #(assoc (assoc (vec (repeat 5 " ")) (first %) "|") (second %) "|") links)]
    (println full-links)
    "Represent State of Board"))

(defn- game-loop [letters links words]
  (represent letters links words)
  "Take Input -> Game Loop")



(defn- initialize [input-puzzle]
  (let [letters (take-nth 2 input-puzzle)
        links (take-nth 2 (rest input-puzzle))
        words (map #(assoc (vec (repeat 5 false)) (first %) (second %)) letters)]
    (println letters)
    (println links)
    (println words)
    (game-loop letters links words)))

(initialize puzzle)
