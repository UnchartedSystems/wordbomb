(ns main
  (:require [clojure.string :as string]))

;; Use Puzzle Input for validation, store word strings as game state.
;; [[4 P] [0 2] [1 N] [3 4] [0 F] [1 2] [3 C] [0 4] [2 A]]
;; "sharP" "sNags" "Frags" "cloCk" "crAnk"
;; Use !*command* inputs to interact with the game
;; eg: !2 = enter row 2. !clear = clears board. !quit = quit game.

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

(def word-set (set (string/split-lines (slurp "words.txt"))))

(defn validate [input words]
  (contains? words input))

(defn toy-puzzle [words]
  (print "Type a word to test the list")
  (flush)
  (validate (string/lower-case (read-line)) words))

(toy-puzzle word-set)
