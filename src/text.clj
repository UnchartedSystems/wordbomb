(ns text
  (:require [generator3 :as g3]
            [clojure.string :as str]
            [utilities :as u]
            [clojure.data.int-map :as i]
            [clojure.term.colors  :as c]))

(def noop-writer
  ;; *out* needs to be bound to a java.io.writer, so proxy a writer
  ;; https://docs.oracle.com/javase/7/docs/api/java/io/Writer.html
  (proxy [java.io.Writer] []
    (close [] nil)
    (flush [] nil)
    (write
      ;; ... which politely ignores any calls.
      ([cbuf] nil)
      ([cbuf off len] nil))))

(defn test-noop []
  (println "Hello!")
  (print "What is your name? ")
  (flush)
  (read-line)
  (binding [*out* noop-writer]
  (println "HELLO!")
  (print "WHAT NAME? ")
  (flush)
  (read-line))
  (println "Finished!"))

;; General

(defn input []
  (doall (print " >> ")
         (flush)))

(defn error-if-seen [func]
  (str "Error on handling '" func "' return"))

(def goodbye (str "Thank you for playing!"))

;; Word & Puzzle Validation
; word-legal?
(defn letter-mismatch [pos word word-letter letter]
  (str "Letter " (inc pos) " of "
       word ": '" word-letter
       "' does not match '" letter "'"))

(defn unmatched-linked
  ([i word word-l adj-l og-pos adj-pos]
   (str "Letter " (inc i) " of " word  " on row " (inc og-pos)
        ": '" word-l "' must match linked '" adj-l
        "' on row " (inc adj-pos) "."))

  ([i word adj-word word-l adj-l og-pos adj-pos]
   (str "Letter " (inc i) " of " word  " on row " (inc og-pos)
        ": '" word-l "' must match linked '" adj-l
        "' in " adj-word " on row " (inc adj-pos) ".")))

(defn matched-unlinked
  ([i word  word-l adj-l og-pos adj-pos]
   (str "Letter " (inc i) " of " word  " on row " (inc og-pos)
        ": '" word-l "' must not match unlinked '" adj-l
        "' on row " (inc adj-pos) "."))

  ([i word adj-word word-l adj-l og-pos adj-pos]
   (str "Letter " (inc i) " of " word  " on row " (inc og-pos)
        ": '" word-l "' must not match unlinked '" adj-l
        "' in " adj-word " on row " (inc adj-pos) ".")))

; is-word?
(def bad-word-len (str "Words must be 5 letters long." "\n"
                       "Commands are prefaced with !"  "\n"
                       "enter !help to see all commands"))

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
                     "------------" "\n"
                    "|"(c/bold "Letterknot") "|" "\n"
                     "------------" "\n"))

(def difficulty (str "New game! Choose difficulty:"   "\n"
                     "Easy / Medium / Hard / Custom"  "\n"
                     "Enter either 1/2/3/c: "))

(def easy   "\nEasy Selected! Have fun :D")
(def normal "\nMedium Selected! Good luck ;)")
(def hard   "\nHard Selected! Fat Chance >:D")
(def debug   (str "\nDebug Selected! \n"
                  "me no likey brain hurty :/"))
(def custom "\nCustom games are a WIP")
(def impossible (str "\nIMPOSSIBLE Selected! \n"
                     (c/red "ψ(｀∇´)ψ HAHAHAHAHAHAHA")))
(def quit "\n Quitting Game! \n")
(def bad-input "\n Bad input! Try again \n")


(def generating "\ngenerating... ")

(defn generated [n-gens t]
  (str "\nGenerated, solved, & analyzed " n-gens " puzzles in " t "ms!" ))

(def post-gen "All that to find the perfect puzzle for you <3")
