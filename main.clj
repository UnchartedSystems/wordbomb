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


(defn represent [board words]
  (let [rows-data (:rows board)
        links-data (:links board)]
    (letfn [(new-line [def-str len] (reduce #(assoc %1 %2 def-str) {} (take len (range))))
            (amend-line [l x y] (assoc l x (str y " ")))
            (convert-row [row] (amend-line (new-line "_ " 5) (first row) (second row)))
            (convert-link [link] (amend-line (amend-line (new-line "  " 5) (first link) "|") (second link) "|"))]
      (let [rows (map #(convert-row %) rows-data)
            links (map #(convert-link %) links-data)
            destruct #(apply str (map second %))]
        (map (fn [r l] (println (destruct r)) (println (destruct l))) rows links)
        ))))

;; MAP WON'T WORK: only limited to shortest collection.,




(defn puzzle [input valid-words]
  (let [board (convert input)
        words {}]
    (represent board words)
    ))

(puzzle prototype word-set)
