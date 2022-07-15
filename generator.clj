(ns generator
  (:require [clojure.string :as str]
            [solver :as solver]))


(def test-puzzle [[4 \P] [0 2] [1 \N] [3 4] [0 \F] [1 2] [3 \C] [0 4] [2 \O]])
(def string-puzzle "4P021N340F123C042O")

(defn- split-pairs [seq]
  (loop [i seq, o []] (if (empty? i) o (recur (subvec i 2) (conj o (subvec i 0 2))))))

(defn- puzzle-str-to-vec [s]
  (let [parse #(if (int? (read-string %)) (read-string %) (get % 0))
        split-pairs #(loop [i %, o []]
                       (if (empty? i) o (recur (subvec i 2) (conj o (subvec i 0 2)))))]
    (split-pairs (mapv parse (str/split s #"")))))

(defn- puzzle-vec-to-str [v] (apply str (flatten v)))

;; ---

;; NOTE: alphabet char range, range end is exclusive
#_(map char (range 65 91))

;; HACK TODO: Just made repeating vector maker to see if helper functions work
;; need to be reworked to incorporate previous numbers to be banned

;; NOTE TODO: # of Solutions isn't good enough! need to measure bottleneck of words across puzzle!
;; A puzzle could have many solutions, but only have 1 possible word in the middle!

;; NOTE REVIEW: This function is very dense, poorly named, and difficult to understand as is. Can be simplified.

(def debug #(do (println %) %))
(defn- generator [len]
  (let [make-new   (fn [v f] (first (filter (fn [i] (not-any? #(= i %) v)) (repeatedly f))))
        new-letter (fn [v] (make-new v #(char (+ 65 (rand-int 25)))))
        new-number (fn [v] (make-new v #(rand-int 5))) ;Not efficient, but also not hotloop
        letter-pairs  (split-pairs (vec (interleave (cycle (take 5 (distinct (repeatedly #(new-number [])))))
                                                    (take len (distinct (repeatedly #(new-letter [])))))))
        #_letter-pos #_(map (fn [i] (vector (first (get letter-pairs i)) (first (get letter-pairs (inc i))))) (range (dec len)))
        #_links      #_(fn [l p] (if (empty? p) l (recur (conj l (vec (take 2 (distinct (repeatedly #(new-number (apply conj (debug (first p)) (debug (last l))))))))) (rest p))))]
    (links [] letter-pos)


    #_(split-pairs (vec (take 8 (interleave (dedupe (repeatedly #(new-number [])))
                                            (dedupe (repeatedly #(new-number [])))))))

    #_(vec (repeatedly len #(vector (new-number []) (new-letter [])))) ; HACK

    #_(loop [c len
             puzzle []
             letters []
             letter-nums []
             last-nums []]
        (if (< c 1)
          puzzle
          (recur ())))
    ))

(generator 5)

