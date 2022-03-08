(ns main
  (:require [clojure.string :as string]))


(def word-set (set (string/split-lines (slurp "words.txt"))))

(def prototype [[4 "P"] [0 2] [1 "N"] [3 4] [0 "F"] [1 2] [3 "C"] [0 4] [2 "A"]])

(defn represent [input words word-len]
  (letfn [(make-line [nil-char spec-char & pos]
            (apply str (for [n (take word-len (range))] (if (some #(= n %) pos) spec-char nil-char))))
          (transform [input]
            (for [n (range (count input))
                  :let [spec (get input n)
                        even-line #(make-line "_" (second spec) (first spec))
                        odd-line #(apply make-line " " "|" spec)]]
              (or (get words (/ n 2)) (if (even? n) (even-line) (odd-line)))))
          (monospace [board] (map #(apply str (interpose " " %)) board))]
    (map println (monospace (transform input)))))


(defn puzzle [input valid-words]
  (let [words {}]
    (represent input (assoc words 3 "TESTS") 5)
    ))

;; (make-line " " "|" spec)
;; (make-line "_" (second spec) (conj [] (first spec)))

(puzzle prototype word-set)
