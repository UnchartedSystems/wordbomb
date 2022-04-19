(ns main
  (:require [clojure.string :as string]))


(def word-set (set (map string/upper-case (string/split-lines (slurp "words.txt")))))

(def prototype [[4 \P] [0 2] [1 \N] [3 4] [0 \F] [1 2] [3 \C] [0 4] [2 \A]])

(def help "Placeholder Help Message")

;; Improvement Ideas:
;; - Should show resulting vertically adjacent linked letters in lowercase
;; - Show something special or satisfying on victory?

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
    (doseq [line (monospace (transform board))]
      (println line))))

;; Validate has multiple steps
;; - Check Input against Row
;; - Create vector of columns
;; - Process a Column to account for Links
;; - Check Column Iteratively
;; - Do this for all columns

(defn- validate [words board]
  (letfn [(to-row [p] (* p 2))
          (to-links [p] (- (* p 2) 1))
          (word-matches-row? [word column letter] (= (get word column) letter))
          (words-match-rows? [] (every? identity (map #(apply word-matches-row? (second %) (board (to-row (first %)))) words)))
          (mk-column [c]
            (reduce #(conj %1 (or (get (words %2) c)
                                   (if (= c (first (board (to-row %2))))
                                     (second (board (to-row %2)))
                                     nil)))
                    [] (range 5)))]     ;WATCH RANGE USED
    (and (words-match-rows?)
         (do (println (map mk-column (range 5))) true)
         ())))


(defn puzzle [board valid-words]
  (letfn [(take-input [words pos]
            (let [input (string/upper-case (read-line))
                  redo #(do (println %) (take-input words pos))
                  all-words (/ (+ (count board) 1) 2)]
              (cond
                ;; Commands
                (= input "!HELP") (redo help)
                (= input "!EXIT") "Game Over"
                (= input "!RESET") (iter {} 0)
                (= input "!CLEAR") (iter (dissoc words pos) pos)
                (number? (read-string input )) (iter words (read-string input))
                (= \! (get input 0)) (redo "Invalid Command")
                ;; Errors
                (< 5 (count input)) (redo "Word Is Too Long")
                (> 5 (count input)) (redo "Word Is Too Short")
                (not (contains? word-set input)) (redo "Not In Word List")
                ;; Validation
                (not (validate (assoc words pos input) board)) (redo "Word Violates Rules")
                (= (count (assoc words pos input)) all-words) (do (represent board (assoc words pos input) pos) (println "YOU WIN"))
                :else (iter (assoc words pos input) (+ pos 1)))))
          (iter [words pos]
            (represent board words pos)
            (take-input words pos))]
    (do (println "\nWelcome to WORDBOMB\nType !help for the rules and controls\n")
        (iter {} 0))))


(puzzle prototype word-set)
