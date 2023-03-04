(ns utilities
  (:require [clojure.string :as str]
            [clojure.data.int-map :as i]))


#_(fs-api/local-connect)
#_(fs-api/instrument-namespaces-clj #{"solver2"})

(def all-words (set (map str/upper-case (str/split-lines (slurp "words-all.txt")))))
(def core-words (set (map str/upper-case (str/split-lines (slurp "words-core.txt")))))

(def test-puzzle [[4 \P] [2 0] [1 \N] [3 4] [0 \F] [1 2] [3 \C] [0 4] [2 \O] [1 3] [4 \R]])
(def string-puzzle "4P021N340F123C042O")

;; NOTE: IDEA! Game should joke about the number of cores you have!
;;        - If you have > ~>=16 threads: 'n threads! THE POWER! :stronglenni'
;;        - If you have < ~<=8  threads: 'maybe this would be faster if your cpu had more than n threads :shrugemoji'
(def n-cpu (.availableProcessors (Runtime/getRuntime)))

(defn- split-pairs [coll]
  (loop [i coll, o []] (if (empty? i) o (recur (subvec i 2) (conj o (subvec i 0 2))))))

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

(defn vecs->intmap [v]
  (let [c (count v)]
    (apply i/int-map (interleave (range c) v) )))

(defn split-puzzle [puzzle]
  [(take-nth 2 puzzle)
   (take-nth 2 (subvec puzzle 1))])

(defn all-frequencies [coll length]
  (mapv (fn [i] (frequencies (mapv #(get % i) coll))) (range length)))

(def core-letter-frequencies (all-frequencies core-words 5))
(def all-letter-frequencies (all-frequencies all-words 5))
