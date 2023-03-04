(ns player2
  (:require [utilities :as utils]
            [generator3 :as g3]
            [clojure.string :as str]
            [utilities :as u]
            [clojure.data.int-map :as i]))

;; NOTE: Planned Improvements:
;;        - Good Rule Checker + Feedback
;;        - Responsive Links (letters on next row)
;;        - New commands!
;;            - !new: start new game
;;            - !help: lists all commands
;;            - !info: Shows # of solutions & bottleneck
;;            - !solve: Retrieves solution using entered words if any
;;        - bug fix: if row-num > # of rows , throw feedback
;;        - On winning a game!
;;            - Show congratz,
;;            - Then show # of unique letters out of possible
;;            - Then Options
;;               - New Game
;;               - Optimize
;;               - Replay
;;               - Info
;;

(def pl println)


;; NOTE: Game loop
;;        - Show representation of board!
;;          - Player position
;;          - Links
;;          - Rows with PRESET & LINKED Letters filled
;;          - Rows with WORD

; FOR ROWS:
; pos = ci? -> '>>' '  '
; IF: word? -> word
;    FOR: i in range 5:
;         When i is in links n,n+1: check: r+1,r-1
;         When i = r-pos -> r-char
;         else: "_"
;
; FOR LINKS:
; FOR i in range 5:
;   if i = link -> "|"
;   else: " "

;; NOTE: - Astericks around preset letters!

(defn- row->str [rows links words i pos]
  (apply str (if (= i pos) " >> " "    ")
         (interpose " "
         (if-some [word (get words i)]
           '(word)
           (mapv (fn [n]
                   (cond
                     (= (nth (get rows i) 0) n) (nth (get rows i) 1)
                     (and (some #(= n %) (get links (dec i))) (get (get words (dec i)) n)) (get (get words (dec i)) n)
                     (and (some #(= n %) (get links i)) (get (get words i) n)) (get (get words (dec i)) n)
                     :else "_")) '(0 1 2 3 4))))))

(defn- link->str [links i]
  (apply str "    "
         (interpose
          " " (mapv (fn [n] (if (some #(= n %) (get links i)) "|" " ")) '(0 1 2 3 4)))))

(defn- represent [rows links words pos]
  (let [[row-str & row-strs]   (mapv #(row->str rows links words % pos) (range (count rows)))
        link-strs  (mapv #(link->str links %) (range (count links)))]
    (pl row-str)
    (mapv pl (interleave link-strs row-strs))))


(defn- game-loop
  ([[rows links]]
   (game-loop (u/vecs->intmap rows) (u/vecs->intmap links) (i/int-map) 0))
  ([rows links words pos]
   (represent rows links words pos)
   ()))


(defn game
  ([] (pl "\n"
          "----------" "\n"
          "Letterknot" "\n"
          "----------" "\n")
   (game true))
  ([_]
   (pl "New game! Choose difficulty:" "\n"
       "Easy / Medium / Hard / Custom")
   (pl "Enter either 1/2/3/c: ")
   (let [i (str/upper-case (read-line))]
     (cond (= i "1") (game 4 6 "\nEasy Selected! Have fun :D")
           (= i "2") (game 5 5 "\nMedium Selected! Good luck ;)")
           (= i "3") (game 6 4 "\nHard Selected! Fat Chance >:D")
           (= i "C") (game 5 5 "\nCustom games are a WIP")
           (= i "Q") (pl "\n Quitting Game! \n")
           (= i "4") (game 10 2 (str "\nIMPOSSIBLE Selected! \n"
                                     "ψ(｀∇´)ψ HAHAHAHAHAHAHA"))
           :else ((pl "\n Bad input! Try again \n") (game true)))))
  ([length bottleneck message]
   (pl message)
   (pl "\ngenerating...")
   (game (g3/make-puzzle-p length bottleneck) "generated!\n"))
  ([puzzle message]
   (pl message)
   (game-loop (u/split-puzzle puzzle))))

(game)
