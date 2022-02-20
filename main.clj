(ns main
  (:require [clojure.string :as string]))

;; Use !*command* inputs to interact with the game
;; eg: !2 = enter row 2. !clear = clears board. !quit = quit game.

;; Game Representation _ _ _ _ P
;;    |   |
;; >> _ N _ _ _
;;          | |
;;    F _ _ _ _
;;      | |
;;    _ _ _ C _
;;    |       |
;;    _ _ A _ _

;; Internal representation of submitted words:
;; {:1 "sharP"
;;  :2 "sNags"
;;  :3 "Flogs"
;;  :4 "cloCk"
;;  :5 "crAnk"}

(def word-set (set (string/split-lines (slurp "words.txt"))))

(defn validate [input words]
  (contains? words input))

(defn toy-puzzle [words]
  (println "Type a word to test the list")
  (println (validate (string/lower-case (read-line)) words)))




;; Use Puzzle Input for validation and representation, store word strings as game state.
;; [[4 P] [0 2] [1 N] [3 4] [0 F] [1 2] [3 C] [0 4] [2 A]]

(def prototype [[4 "P"] [0 2] [1 "N"] [3 4] [0 "F"] [1 2] [3 "C"] [0 4] [2 "A"]])

;; Game Representation
;;    _ _ _ _ P
;;    |   |
;; >> _ N _ _ _
;;          | |
;;    F _ _ _ _
;;      | |
;;    _ _ _ C _
;;    |       |
;;    _ _ A _ _

(defn convert [input]
  (hash-map :rows (take-nth 2 input)
            :links (keep-indexed #(if (odd? %1) %2) input)))

(defn convert-row [row empty]
  ())

(defn convert-link [link])

(defn represent [board words]
  (let [s ""
        rows (:rows board)
        links (:links board)
        line-map {0 "_ "
                  1 "_ "
                  2 "_ "
                  3 "_ "
                  4 "_ "}]
    (map convert-row rows)
    (map convert-link links)
    ))

(defn puzzle [input valid-words]
  (let [board (convert input)
        words {}]
    (represent board words)
    (print board)
    ))

(puzzle prototype word-set)
