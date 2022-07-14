(ns generator
  (:require [clojure.string :as str]))


(def test-puzzle [[4 \P] [0 2] [1 \N] [3 4] [0 \F] [1 2] [3 \C] [0 4] [2 \O]])
(def string-puzzle "4P021N340F123C042O")

(defn- puzzle-str-to-vec [s]
  (let [parse #(if (int? (read-string %)) (read-string %) (get % 0))
        split-pairs #(loop [i %, o []]
                       (if (empty? i) o (recur (subvec i 2) (conj o (subvec i 0 2)))))]
    (split-pairs (mapv parse (str/split s #"")))))

(defn- puzzle-vec-to-str [v] (apply str (flatten v)))
