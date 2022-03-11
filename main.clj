(ns main
  (:require [clojure.string :as string]))


(def word-set (set (map string/upper-case (string/split-lines (slurp "words.txt")))))

(def prototype [[4 "P"] [0 2] [1 "N"] [3 4] [0 "F"] [1 2] [3 "C"] [0 4] [2 "A"]])

(def help "Placeholder Help Message")

(defn represent [board words pos]
  (letfn [(monospace [board]
            (map #(apply str (interpose " " %)) board))
          (make-line [l default char & spec]
            (apply str (for [c (take 5 (range))] (if (some #(= c %) spec) char default))))
          (transform [board]
            (for [l (range (count board))
                  :let [spec (get board l)
                        even (apply vector "_" (reverse spec))
                        odd  (apply vector " " "|" spec)]]
              (str (if (= l (* 2 pos)) ">> " "   ")
                   (or (get words (/ l 2)) (apply make-line l (if (even? l) even odd))))))]
    (println)
    (doseq [line (monospace (transform board))]
      (println line))))

;; Returns a bool if word passes rules
(defn validate [input pos board words]
  (let [upper-row   (get board (- (* pos 2) 2))
        upper-links (get board (- (* pos 2) 1))
        upper-word  (get words (- pos 1))]
    (and (not (and upper-links upper-row))
         )))

(defn puzzle [board valid-words]
  (letfn [(start []
            (do (println "Welcome to WORDBOMB")
                (println "Type !help for the rules and controls")
                (println)))
          (take-input [words pos]
            (let [input (string/upper-case (read-line))
                  redo #(do (println %) (take-input words pos))]
              (cond (= input "!HELP") (redo help)
                    (= input "!EXIT") "Game Over"
                    (= input "!RESET") (iter {} 0)
                    (= input "!CLEAR") (iter (dissoc words pos) pos)
                    (number? (read-string input )) (iter words (read-string input))
                    (= \! (get input 0)) (redo "Invalid Command")
                    (< 5 (count input)) (redo "Word Is Too Long")
                    (> 5 (count input)) (redo "Word Is Too Short")
                    (not (contains? word-set input)) (redo "Not In Word List")
                    (not (validate input pos board words)) (redo "Word Violates Rules")
                    :else (iter (assoc words pos input) (+ pos 1)))))
          (iter [words pos]
            (represent board words pos)
            (take-input words pos))]
    (do (start)
        (iter {} 0))))


(puzzle prototype word-set)
