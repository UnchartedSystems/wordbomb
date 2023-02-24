(ns utilities
  (:require [clojure.string :as str]))


#_(fs-api/local-connect)
#_(fs-api/instrument-namespaces-clj #{"solver2"})

(def all-words (set (map str/upper-case (str/split-lines (slurp "words-all.txt")))))
(def core-words (set (map str/upper-case (str/split-lines (slurp "words-core.txt")))))

(def test-puzzle [[4 \P] [2 0] [1 \N] [3 4] [0 \F] [1 2] [3 \C] [0 4] [2 \O] [1 3] [4 \R]])
(def string-puzzle "4P021N340F123C042O")

(defn- split-pairs [seq]
  (loop [i seq, o []] (if (empty? i) o (recur (subvec i 2) (conj o (subvec i 0 2))))))

(defn- puzzle-str-to-vec [s]
  (let [parse #(if (int? (read-string %)) (read-string %) (get % 0))
        split-pairs #(loop [i %, o []]
                       (if (empty? i) o (recur (subvec i 2) (conj o (subvec i 0 2)))))]
    (split-pairs (mapv parse (str/split s #"")))))

(defn- puzzle-vec-to-str [v] (apply str (flatten v)))


;; NOTE: testing set perf on strings
(defn make-word
  ([] (make-word 26))
  ([alph]
   (let [get-char #(char (+ 65 (rand alph)))]
     (apply str (take 5 (repeatedly get-char))))))
