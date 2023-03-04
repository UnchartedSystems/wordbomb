(ns generator3
  (:require [utilities :as u]
            [solver4 :as s4]))

#_[[1]
   [0 4]
   [2]
   [3 1]
   [0]
   [2 4]
   [3]
   [0 1]
   [4]
   [2 3]
   [0]
   [1 4]
   [2]]

;; NOTE: method of generating:
;;       entry by entry, based on the last two entries
;;       letters will have two choices (3 constraints + 1 position)
;;          - Should equalize frequency
;;       links will have no choices (3 constraints + 2 links)


;; TODO: stupid implementation for now
(defn- get-letter []
  (let [alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZ"]
    (cycle (shuffle (map #(get alphabet %) (range 26))))))

#_(get-letter)

;; TODO: stupid implementation for now, needs noncyclical randomness
;; (defn- get-positions [len]
;;   (take len (cycle (shuffle (range 5)))))

(filterv #(distinct? 1 2 %) '(2 3 1))

(defn- do-smthng [& prev-entries]
  (let [constraints (keep identity prev-entries)]
    (vec (take 2 (shuffle (filter #(apply distinct? % constraints)
                                     '(0 1 2 3 4)))))))
;; TODO: stupid, does not eq
(defn- make-row [[pos-1 pos-2] letters]
  [pos-1 (first (filter #(apply distinct? % letters) (get-letter)))]) ;; Will hang if length > 26

(defn- generate [coll counter]
  (if (not (pos? counter))
    coll
    (let [[link1 link2]  (peek (pop coll))
          [pos _]        (peek coll)
          letters        (map second (take-nth 2 coll))
          links          (do-smthng pos link1 link2)
          row            (make-row (do-smthng (nth links 0) (nth links 1) pos) letters)]
      (recur (conj coll links row) (dec counter)))))

(defn bottlenecks? [puzzle length]
  (map #(count (keys %))
       (u/all-frequencies
        (s4/solutions puzzle u/core-words)
        length)))

;; Old version! mostly single threaded with brief spurts of multithreading, not as effective!
(defn make-puzzle [length bottleneck]
  (let [puzzle (generate [(make-row (do-smthng) '())] (dec length))]
    (if (not-empty puzzle)
      (if (<= bottleneck (apply min (bottlenecks? puzzle length)))
        puzzle
        (recur length bottleneck))
      (recur length bottleneck))))

(defn make-puzzle-iter [length bottleneck result]
  (let [puzzle (generate [(make-row (do-smthng) '())] (dec length))]
    (when-not (realized? result)
      (if (not-empty puzzle)
        (if (<= bottleneck (apply min (bottlenecks? puzzle length)))
          (deliver result puzzle)
          (recur length bottleneck result))
        (recur length bottleneck result)))))

(defn make-puzzle-p [length bottleneck]
  (let [result (promise)]
    (future (doall (apply pcalls (repeat u/n-cpu  #(make-puzzle-iter length bottleneck result)))))
    @result))

