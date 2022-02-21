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


(defn make-new-line [placeholder length]
  (let [nums (take length (range))
        result {}]
    (reduce #(assoc %1 %2 placeholder) result nums)))

(defn represent [board words]
  (let [rows-data (:rows board)
        links-data (:links board)
        line (make-new-line "_ " 5)]
    (letfn [(make-line [l x y] (assoc l x (str y " ")))
            (convert-row [row] (make-line line (first row) (second row)))
            (convert-link [link] (make-line (make-line line (first link) "|") (second link) "|"))]
      (println "test")
      (let [rows (map #(convert-row %) rows-data)
            links (map #(convert-link %) links-data)]
        (map (fn [r l] ((apply #(println (second %)) r) (apply #(println (second %)) l))) rows links)))))




(defn puzzle [input valid-words]
  (let [board (convert input)
        words {}]
    (represent board words)
    ))

(puzzle prototype word-set)
