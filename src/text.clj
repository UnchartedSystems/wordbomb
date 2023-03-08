(ns text
  (:require [generator3 :as g3]
            [clojure.string :as str]
            [utilities :as u]
            [clojure.data.int-map :as i]
            [clojure.term.colors  :as c]))

;; General

(defn error-if-seen [func]
  (str "Error on handling '" func "' return"))

;; Word & Puzzle Validation
(def bad-word-len (str "Words must be 5 letters long." "\n"
                       "Commands are prefaced with !"  "\n"
                       "enter !help to see all available commands"))

(defn unrecognized-word [!word]
  (str !word " is not a word Letterknot recognizes" "\n"))

;; Game Loop
; commands
(defn bad-command [c]
  (str "Command: " c " not recognized." "\n"
       "Use !help to see all available commands"))

; row switching
(defn bad-row-# [n rows-#]
  (str "Row " n " does not exist." "\n"
       "You can switch to Rows 1 - " rows-#))

;; Game Initialization

(def letterknot (str "\n"
                     "----------" "\n"
                     (c/white "Letterknot") "\n"
                     "----------" "\n"))

(def difficulty (str "New game! Choose difficulty:"   "\n"
                     "Easy / Medium / Hard / Custom"  "\n"
                     "Enter either 1/2/3/c: "))

(def easy   "\nEasy Selected! Have fun :D")
(def normal "\nMedium Selected! Good luck ;)")
(def hard   "\nHard Selected! Fat Chance >:D")
(def custom "\nCustom games are a WIP")
(def impossible (str "\nIMPOSSIBLE Selected! \n"
                     (c/red "ψ(｀∇´)ψ HAHAHAHAHAHAHA")))
(def quit "\n Quitting Game! \n")
(def bad-input "\n Bad input! Try again \n")


(def generating "\ngenerating... ")

(defn generated [n-gens t]
  (str "\nGenerated, solved, & analyzed " n-gens " puzzles in " t "ms!" ))

(def post-gen "All that to find the perfect puzzle for you <3")
