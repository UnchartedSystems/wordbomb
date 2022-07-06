(ns solver
  (:require [clojure.string :as string]))

;; 1 - Filter wordset for letter positions

(def word-set (set (map string/upper-case (string/split-lines (slurp "words.txt")))))
(def puzzle [[4 \P] [0 2] [1 \N] [3 4] [0 \F] [1 2] [3 \C] [0 4] [2 \A]])
;; "4P021N340F123C042A"

(defn solve [puzzle]
  (let [letters (vec (take-nth 2 puzzle))
        row-words (map (fn [[n l]] (filter #(= (get % n) l) word-set)) letters)]
    row-words))

(solve puzzle)
