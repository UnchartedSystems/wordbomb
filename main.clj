(ns main
  (:require [clojure.string :as string]))


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

(def word-set (set (map string/upper-case (string/split-lines (slurp "words.txt")))))

(def puzzle [[4 \P] [0 2] [1 \N] [3 4] [0 \F] [1 2] [3 \C] [0 4] [2 \A]])

(defn- represent [default words links pos]
  (let [full-words (map #(or (words %) (default %)) (range (count default)))
        links-full (map (fn [l-row] (reduce #(assoc %1 %2 "|") (vec (repeat 5 " ")) l-row)) links)
        words-full (map (fn [w-row] (map #(if (not %) "_" %) w-row)) full-words)
        links-str (map #(reduce str %) links-full)
        words-str (map #(reduce str %) words-full)
        representation (cons (first words-str) (interleave links-str (rest words-str)))
        position #(if (= (* pos 2) %) ">>" "  ")]
    (println "")
    (mapv #(apply println (position %2) %1) representation (range (count representation)))))


(defn- validate [letters links words input-row input-str pos]
  (let [new-words (assoc words pos input-row)
        word #(get new-words %)]
    (and (contains? word-set input-str)
         (= (input-row (first (letters pos))) (second (letters pos)))
         (reduce (fn [b i] (if (not (and (word i) (word (inc i)))) b
                               (reduce #(and %1 %2) b (map #(= (= %1 %2) (not (apply distinct? %3 (links i))))
                                                           (word i)
                                                           (word (inc i))
                                                           (range)))))
                 true (range (count links))))))


(defn- game-loop [letters links default words pos show?]
  (if show? (represent default words links pos))
  (let [input-str (string/upper-case (read-line))
        input-row (vec (char-array input-str))
        iter  (partial game-loop letters links default)]
    (cond
      (= input-str "!EXIT")  "Game Over"
      (= input-str "!RESET")  (iter {} 0 true)
      (= input-str "!CLEAR")  (iter (dissoc words pos) pos true)
      (number? (read-string input-str))  (iter words (dec (read-string input-str)) true)
      (not (validate letters links words input-row input-str pos))  (do (println "Word Violates Rules") (iter words pos false))
      (= (count (assoc words pos input-row)) (count default))  (do (represent default (assoc words pos input-row) links pos) (println "YOU WIN"))
      :else (iter (assoc words pos input-row )  (+ pos 1) true))))


(defn- initialize [input-puzzle]
  (let [letters (vec (take-nth 2 input-puzzle))
        links (vec (take-nth 2 (rest input-puzzle)))
        default (vec (map #(assoc (vec (repeat 5 false)) (first %) (second %)) letters))]
    (game-loop letters links default {} 0 true)))

(initialize puzzle)
