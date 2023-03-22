(ns player2
  (:require [utilities :as u]
            [text :as t]
            [generator3 :as g]
            [clojure.string :as str]
            [clojure.data.int-map :as i]
            [clojure.term.colors  :as c]))

;; NOTE: Planned Improvements:
;;        - On winning a game!
;;            - Show congratz,
;;            - Then show # of unique letters out of possible
;;            - Then Options
;;               - New Game
;;               - Optimize
;;               - Replay
;;               - Info
;;
;; NOTE: player specific utilities for fast printing
;; NOTE: NEW IDEA TODO: seperate file for errors / strings fns with color
(def pl println)
(defmacro pl-do [& args]
  `(do (println (str ~@(drop-last args)))
       ~(last args)))

(defn or= [val & matches]
  (if (some #(= val %) matches)
    true false))

(defn- !help []
  (pl "PICK ROW USING NUM TEXT")
  (pl "NOTE: you can also use shorthands like !c !")
  (pl "use bolded letter to denote shorthand letter")
  (pl "w + s should move up or down one row")
  (mapv #(apply pl %)
   (partition 2
     '("!learn" "- learn how Letterknot is played"
       "!show"  "- shows the puzzle in its current state"
       "!clear" "- erase the word on the selected row"
       "!reset" "- erase all rows & return the puzzle to a clean slate"
       "!info"  "- shows information about the current puzzle"
       "!hint"  "- get a hint based on the current state of the puzzle"
       "!solve" "- solves the puzzle using its current state, if possible"
       "!new"   "- abandon the puzzle & start a new puzzle"
       "!quit"  "- abandon the puzzle & quit Letterknot"
       ))))



; TODO: FIXME: CHANT hasn't been added as a word yet, but all letters are implied
;       TODO: solution! make implied letters light grey!
 ;;    P L A N E
 ;;        | |
 ;; >> C H A N T
 ;;    | |
 ;;    C H E E R
 ;;          | |
 ;;    B _ _ E R

(defn- row->str [rows links words i pos]
  (apply str (if (= i pos) " >> " "    ")
         (interpose " "
         (if-some [word (get words i)]
           word
           (mapv (fn [n]
                   (cond
                     (= (nth (get rows i) 0) n) (nth (get rows i) 1)
                     (and (some #(= n %) (get links (dec i))) (get (get words (dec i)) n)) (get (get words (dec i)) n)
                     (and (some #(= n %) (get links i)) (get (get words (inc i)) n)) (get (get words (inc i)) n)
                     :else "_")) '(0 1 2 3 4))))))


(defn- link->str [links i]
  (apply str "    "
         (interpose
          " " (mapv (fn [n] (if (some #(= n %) (get links i)) "|" " ")) '(0 1 2 3 4)))))

(defn- represent [rows links words pos]
  (let [[row-str & row-strs]   (mapv #(row->str rows links words % pos) (range (count rows)))
        link-strs  (mapv #(link->str links %) (range (count links)))]
    (pl "\n")
    (pl row-str)
    (mapv pl (interleave link-strs row-strs))
    (pl "\n")))


;; TODO: check for:
;; Word conflicts with +-1 WORD
;; [word link adj-word]
;;  Every Letter:
;;    linked:  = l1 l2
;;    nolink: != l1 l2
;; Word conflicts with +-1 ROW
;; [word link adj-row]
;;  For l2 position:
;;    linked:  = l1 l2  ;NOTE: this may signify an error!
;;    nolink: != l1 l2
;; Word conflicts with +-2 WORD
;; [word link far-word]
;;  Every Letter:
;;    linked: != l1 l2
;;    nolink: TRUE
;; Word conflicts with +-2 ROW
;; [word link far-row]
;;  For l2 position:
;;    linked: != l1 l2
;;    nolink: TRUE

(defn- linked? [position [link-1 link-2]]
  (or (= position link-1) (= position link-2)))

(defn- !far-word [word far-word links]
  (when far-word
    (loop [position 0]
      (when (> 5 position)
        (let [w-letter (get word position)
              f-letter (get far-word position)]
          (when (linked? position links)
            (if (not= w-letter f-letter)
              (recur (inc position))
              (pl-do "FAR LINKED CANNOT MATCH: " w-letter f-letter true))))))))

(defn- !adj-word [word adj-word links]
  (when adj-word
    (loop [position 0]
      (when (> 5 position)
        (let [w-letter (get word position)
              a-letter (get adj-word position)]
          (if (linked? position links)
            (if (= w-letter a-letter)
              (recur (inc position))
              (pl-do "LINKED DO NOT MATCH: " w-letter a-letter true))
            (if (not= w-letter a-letter)
              (recur (inc position))
              (pl-do "UNLINKED CANNOT MATCH: " w-letter a-letter true))))))))

(defn- !far-row [word [position letter] links]
  (when letter
    (let [w-letter (get word position)]
      (when (linked? position links)
        (if (not= w-letter letter) false
          (pl-do "FAR-LETTER CANNOT MATCH: " w-letter letter true))))))

(defn- !adj-row [word [position letter] links]
  (when letter
    (let [w-letter (get word position)]
      (if (linked? position links)
        (pl-do "ERROR: linked row-letter!?" true)
        (if (not= w-letter letter) false
            (pl-do " ADJ-LETTER CANNOT MATCH: " w-letter letter true))))))
=
;TODO these can be abstracted better
(defn- !adj-rows [word rows links pos]
  (or (!adj-row word (get rows (inc pos))  (get links pos))
      (!adj-row word (get rows (dec pos))  (get links (dec pos)))))

(defn- !adj-words [word words links pos]
  (or (!adj-word word (get words (inc pos)) (get links pos))
      (!adj-word word (get words (dec pos)) (get links (dec pos)))))

(defn- !far-rows [word rows links pos]
  (or (!far-row word (get rows (+ pos 2)) (get links (inc pos)))
      (!far-row word (get rows (+ pos 2)) (get links pos))
      (!far-row word (get rows (- pos 2)) (get links (dec pos)))
      (!far-row word (get rows (- pos 2)) (get links (- pos 2)))))

(defn- !far-words [word words links pos]
  (or (!far-word word (get words (+ pos 2)) (get links (inc pos)))
      (!far-word word (get words (+ pos 2)) (get links pos))
      (!far-word word (get words (- pos 2)) (get links (dec pos)))
      (!far-word word (get words (- pos 2)) (get links (- pos 2)))))

(defn- !row-letter [word [position letter]]
  (let [w-letter (get word position)]
    (when (not= letter w-letter)
      (pl-do (t/letter-mismatch position word w-letter letter) true))))

 ;; FIXME: representation AND link checker fail!
 ;; NOTE: reason for link: !far-word not checking both
 ;        sets of links between 1 & 2
 ;;    C L A I M
 ;;        | |
 ;;    _ _ A I T
 ;;    | |
 ;; >> C H E E R
 ;;          | |
 ;;    B _ _ E R

;; NOTE: Every rule involves checking linked?
;;       adj-letter & adj-word have same conditions
;;       Same for far-letter & far-word
;;       eg: adj-word, adj-letter, far-letter, far-word all pass linked & unlinked functions
;;       into a main function that checks if linked and then applies the appropriate function
;;       and returns two bools: one for if linked, and another for if passed.
;;       then original function actually does the error reporting


(defn- right-size? [input]
  (if (= 5 (count input)) true
      (pl-do t/bad-word-len false)))

(defn- is-word? [input]
  (if-not (apply distinct? input u/all-words) true
          (pl-do (t/unrecognized-word input) false)))

(defn next-pos [pos rows words]
  (let [rows   (doall (range (count rows)))
        filled (keys words)
        rows-left (filterv #(apply distinct? % filled) rows)]
    (if (empty? rows-left)
      false
      (if-let [next (some #(when (<= % pos) %) rows-left)]
        next
        (first rows-left)))))

(defn- parse-pos [digits rows]
  (let [num (read-string digits)]
    (if (and (<= num (count rows)) (> num 0))
      (dec num)
      (pl-do (t/bad-row-# num (count rows)) false))))


(def current-puzzle (atom nil))

(defn- game-loop
  ([puzzle]
   (let [[rows links] (u/split-puzzle puzzle)]
     (reset! current-puzzle puzzle)
     (game-loop (u/vecs->intmap rows) (u/vecs->intmap links) (i/int-map) 0 true)))
  ([rows links words pos show?]
   (when show? (represent rows links words pos))
   (t/input)
   (when-let [raw-input (read-line)]
     (let [i (str/upper-case raw-input)]
       (cond (= "" i)         (recur rows links words pos false)

             (= (first i) \!) (cond (or= i "!RULES")  (pl-do t/rules (recur rows links words pos false))
                                    (or= i "!SHOW" "!S")   (recur rows links words pos true)
                                    (or= i "!CLEAR" "!C")  (recur rows links (dissoc words pos) pos true)
                                    (or= i "!RESET" "!R")  (recur rows links (i/int-map) pos true)
                                    ;; (or= i "!UNDO" "!U")   ()
                                    (or= i "!HELP")   (do (!help) (recur rows links words pos true))
                                    (or= i "!INFO")   (pl-do (t/puzzle-info @current-puzzle) (recur rows links words pos false))
                                    ;; (or= i "!HINT" "!H")   ()
                                    ;; (or= i "!SOLVE")  ()
                                    (or= i "!NEW" "!N")    :new-game
                                    (or= i "!QUIT" "!Q")   :quit
                                    :else           (pl-do (t/bad-command i)
                                                           (recur rows links words pos false)))

             (every? #(Character/isDigit %) i) (if-let [new-pos (parse-pos i rows)]
                                                 (recur rows links words new-pos true)
                                                 (recur rows links words pos false))

             (not (right-size? i))            (recur rows links words pos false)
             (not (is-word? i))               (recur rows links words pos false)
             (!row-letter i (get rows pos))   (recur rows links words pos false)
             (!adj-rows   i rows links pos)   (recur rows links words pos false)
             (!adj-words  i words links pos)  (recur rows links words pos false)
             (!far-rows   i rows links pos)   (recur rows links words pos false)
             (!far-words  i words links pos)  (recur rows links words pos false)

             :else (let [new-words (assoc words pos i)
                         won? (= (count new-words) (count rows))]
                     (if won?
                       (do (represent rows links new-words pos)
                           (pl "You Win!")
                           :quit)
                       (recur rows links new-words (next-pos pos rows new-words) true))))))))


(def bug [[1 \L] [2 3] [4 \T] [0 1] [2 \E] [3 4] [0 \B]])

(defn game
  ([]
   (pl t/letterknot)
   (game true))

  ([_]
   (pl t/difficulty)
   (t/input)
   (when-let [raw-input (read-line)]
     (let [i (str/upper-case raw-input)]
       (cond (= i "1") (game 4 15 t/easy)
             (= i "2") (game 5 10 t/normal)
             (= i "3") (game 6 5 t/hard)
             (= i "C") (game 5 10 t/custom)
             (= i "9") (game 9 2 t/impossible)
             (= i "D") (game 4 35 t/debug)
             (= i "Q") (pl t/quit)
             :else     (pl-do t/bad-input
                              (game true))))))

  ;; TODO: make 'generating...' only appear after a set time, like 5seconds
  ([length bottleneck message]
   (pl message)
   #_(pl t/generating)
   (let [t-before (. System nanoTime)
         [puzzle n-gens] (g/make-puzzle-p length bottleneck)
         t-after (. System nanoTime)
         t (str (int (/ (- t-after t-before) 1000000.0)))
         gen-message  (t/generated n-gens t)]
     (pl-do gen-message
            (game puzzle t/post-gen))))

  ([puzzle message]
   (pl message)
   (let [ng? (game-loop puzzle)]
     (cond (= ng? :restart)  (recur puzzle "Restarting!")
           (= ng? :new-game) (game true)
           (= ng? :quit)     (pl t/goodbye)
           :else             (pl t/goodbye)))))

#_(game)
