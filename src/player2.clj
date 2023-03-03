(ns player2
  (:require [utilities :as utils]
            [generator3 :as g3]
            [clojure.string :as str]))

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


(defn- represent [rows links words pos]
  )

(defn- game-loop [puzzle]
  )


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
   (game (g3/make-puzzle length bottleneck) "generated!"))
  ([puzzle message]
   (pl message)
   (game-loop puzzle)))

(game)
