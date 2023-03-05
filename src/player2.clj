(ns player2
  (:require [utilities :as utils]
            [generator3 :as g3]
            [clojure.string :as str]
            [utilities :as u]
            [clojure.data.int-map :as i]
            [clojure.term.colors  :as c]))

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


(defn- !help []
  (mapv #(apply pl %)
   (partition 2
     '("!rules" "- learn how Letterknot is played"
       "!quit"  "- abandon the puzzle & quit Letterknot"
       "!clear" "- erase the word on the selected row"
       "!reset" "- erase all rows & return the puzzle to a clean slate"
       "!new"   "- abandon the puzzle & start a new puzzle"
       "!show"  "- shows the puzzle in its current state"
       "!info"  "- shows information about the puzzle"
       "!hint"  "- get a hint based on the current state of the puzzle"
       "!solve" "- solves the puzzle using its current state, if possible"))))

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
          (c/white "Letterknot") "\n"
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
  ;; TODO: make 'generating...' only appear after a set time, like 5seconds
  ([length bottleneck message]
   (pl message)
   (pl "\ngenerating... ")
   (let [t-before (. System nanoTime)
         [puzzle n-gens] (g3/make-puzzle-p length bottleneck)
         t-after (. System nanoTime)
         t (str (int (/ (- t-after t-before) 1000000.0)))
         gen-message   (str"Generated, solved, & analyzed " n-gens " puzzles in " t "ms!\n" )
         next-message "All that to find the perfect puzzle for you <3\n"]
     (print gen-message)
     (game puzzle next-message)))
  ([puzzle message]
   (pl message)
   (game-loop (u/split-puzzle puzzle))))

(game)
