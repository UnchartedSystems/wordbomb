(ns generator
  (:require [clojure.string :as str]
            [solver :as solver]))


(def test-puzzle [[4 \P] [0 2] [1 \N] [3 4] [0 \F] [1 2] [3 \C] [0 4] [2 \O]])
(def string-puzzle "4P021N340F123C042O")

(defn- puzzle-str-to-vec [s]
  (let [parse #(if (int? (read-string %)) (read-string %) (get % 0))
        split-pairs #(loop [i %, o []]
                       (if (empty? i) o (recur (subvec i 2) (conj o (subvec i 0 2)))))]
    (split-pairs (mapv parse (str/split s #"")))))

(defn- puzzle-vec-to-str [v] (apply str (flatten v)))

;; ---

;; NOTE: alphabet char range
#_(not-any? #(= \N %)  [\N \F \P])

;; HACK TODO: Just made repeating vector maker to see if helper functions work
;; need to be reworked to incorporate previous numbers to be banned
(defn- generator [n len]
  (let [make-new   (fn [v f] (first (filter (fn [i] (not-any? #(= i %) v)) (repeatedly f))))
        new-letter (fn [v] (make-new v #(char (+ 65 (rand-int 25)))))
        new-number (fn [v] (make-new v #(rand-int 5)))]
    (vec (repeatedly len #(vector (new-number []) (new-letter [])))))) ; HACK

(generator 1 5)
