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

(defn- row->str [rows links words i pos]
  (apply str (if (= i pos) " >> " "    ")
         (interpose " "
         (if-some [word (get words i)]
           word
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
    (pl "\n")
    (pl row-str)
    (mapv pl (interleave link-strs row-strs))))


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
;;
;; NOTE: Every rule involves checking linked?
;;       adj-letter & adj-word have same conditions
;;       Same for far-letter & far-word
;;       eg: adj-word, adj-letter, far-letter, far-word all pass linked & unlinked functions
;;       into a main function that checks if linked and then applies the appropriate function
;;       and returns two bools: one for if linked, and another for if passed.
;;       then original function actually does the error reporting

(defn per-letter? [word-l adj-l pos-l [l1 l2]]
  (let [linked? (or (= pos-l l1) (= pos-l l2))]
    (cond (and linked? (not= word-l adj-l))     (pl-do "FIXME LINK " word-l adj-l  false)
          (and (not linked?) (= word-l adj-l))  (pl-do "FIXME NOLINK " word-l adj-l false)
          :else true)))

(defn adj-word? [word adj-word link]
   (if-not adj-word true
           (loop [pos-l 0]
             (if (>= pos-l 5) true
                 (let [word-l  (get word pos-l)
                       adj-l   (get adj-word pos-l)]
                   (if-not (per-letter? word-l adj-l pos-l link)
                     false
                     (recur (inc pos-l))))))))

(defn- adj-letter?
  ([word [pos-l letter]]
   (if-not letter true
     (let [word-l (get word pos-l)]
       (if (not= word-l letter) true
           (pl-do "ADJ-LETTER" false)))))
  ([word [pos-l letter] [l1 l2]]
   (if-not letter true
     (let [word-l (get word pos-l)]
       (if (and (not= word-l letter)
                (or (= l1 pos-l)
                    (= l2 pos-l)))
         true
           (pl-do "FAR-LETTER" false))))))

(defn- row-letter? [word rows pos-r]
  (let [[pos-l letter] (get rows pos-r)
        word-l        (get word pos-l)]
    (if (= letter word-l) true
        (pl-do (t/letter-mismatch pos-l word word-l letter) false))))

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

(defn- game-loop
  ([[rows links]]
   (game-loop (u/vecs->intmap rows) (u/vecs->intmap links) (i/int-map) 0 true))
  ([rows links words pos show?]
   (when show? (represent rows links words pos))
   (when-let [raw-input (read-line)]
     (let [i (str/upper-case raw-input)]
       (cond (= "" i)         (recur rows links words pos false)

             (= (first i) \!) (cond (= i "!RULES")  ()
                                    (= i "!SHOW")   (recur rows links words pos true)
                                    (= i "!CLEAR")  (recur rows links (dissoc words pos) pos true)
                                    (= i "!RESET")  (recur rows links (i/int-map) pos true)
                                    (= i "!UNDO")   ()
                                    (= i "!HELP")   (do (!help) (recur rows links words pos true))
                                    (= i "!INFO")   ()
                                    (= i "!HINT")   ()
                                    (= i "!SOLVE")  ()
                                    (= i "!NEW")    :new-game
                                    (= i "!QUIT")   ()
                                    :else           (pl-do (t/bad-command i)
                                                           (recur rows links words pos false)))

             (every? #(Character/isDigit %) i) (if-let [new-pos (parse-pos i rows)]
                                                 (recur rows links words new-pos true)
                                                 (recur rows links words pos false))

             (not (right-size? i))                                                (recur rows links words pos false)
             (not (is-word? i))                                                   (recur rows links words pos false)
             (not (row-letter? i rows pos))                                       (recur rows links words pos false)
             (not (adj-letter? i (get rows (inc pos))))                           (recur rows links words pos false)
             (not (adj-letter? i (get rows (dec pos))))                           (recur rows links words pos false)
             (not (adj-word? i (get words (inc pos)) (get links pos)))            (recur rows links words pos false)
             (not (adj-word? i (get words (dec pos)) (get links (dec pos))))      (recur rows links words pos false)
             (not (adj-letter? i (get rows (+ pos 2)) (get links pos)))           (recur rows links words pos false)
             (not (adj-letter? i (get rows (- pos 2)) (get links (dec pos))))     (recur rows links words pos false)
             ;BUG: HACK: TODO: These check for row+-2, but check for linked matches
             ;                 and unlinked nonmatches! We can't check unlinked nonmatches
             ;; (not (adj-word? i (get words (+ pos 2)) (get links pos) false))       (recur rows links words pos false)
             ;; (not (adj-word? i (get words (- pos 2)) (get links (dec pos)) false)) (recur rows links words pos false)
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
            (game bug t/post-gen))))

  ([puzzle message]
   (pl message)
   (let [ng? (game-loop (u/split-puzzle puzzle))]
     (cond (= ng? :restart)  (recur puzzle "Restarting!")
           (= ng? :new-game) (game true)
           (= ng? :quit)     (pl t/goodbye)
           :else             (pl t/goodbye)))))


(game)
