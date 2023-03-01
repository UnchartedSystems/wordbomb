(ns solver5)

;; NOTE: For future use.
;;       First idea is to convert get-linksets to a lazy transducer!
;;       Then selective eval + caching comes free
;;       Also: make DFS return its output and memoize recursive evals!
;;       This way redundant permutations come free


;; NOTE: Design a permutations accumulator that:
;;       Can be multithreaded
;;       Only does needed work!
;;          - This includes upstream work!
;;              - lazy eval vs eager eval
;;          - When +: use cache > recompute work
;;       Can be cached efficiently
;;       List v Vec for output
;;          - List makes more sense!
;;            tree leaves will be initial element
;;            earlier words will be added after & be earlier
;;
;; NOTE: Should be depth first:
;;          - Allows deep cache to build!
;;

;; NOTE: Do not pass complex datastructures as entries into memoized functions
;;       OR
;;       Use only simple args on Caching

;; NOTE: DFS design
;; Is rows empty?
;;    yes - return word in a list
;;    no  -
;;      Generate Wordset
;;      Is wordset empty?
;;          yes - return nil
;;          no  -
;;            For each word -> recur
;;            Process return into '(("word" "word" "word") ("word" "word" "word") ("word" "word" "word"))
;;            cache it

(defn- get-rowsets [rows wordset]
  (mapv (fn [[i l]] (filterv #(= (get % i) l) wordset)) rows))

(defn- get-linkset [word rowset [link-1 link-2]]
  (letfn [(compatible? [^java.lang.String w]
            (loop [i 0]
              (when (if (or (= link-1 i) (= link-2 i))
                      (.equals (nth word i) (nth w i))
                      (not (.equals (nth word i) (nth w i))))
                (if (= i 4) w (recur (inc i))))))]
    [word (filterv compatible? rowset)]))

(defn get-linksets [row next-row link]
  (persistent!
   (transduce (comp (map #(get-linkset % next-row link))
                    (filter #(not-empty (peek %))))
              conj! (transient {}) row)))
