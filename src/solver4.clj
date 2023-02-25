(ns solver4)

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

(defn- dfs-old [solutions linksets wordlinks path]
   (let [word       (first wordlinks)
         adj-words  (second wordlinks)
         path       (conj path word)]
       (doseq [w adj-words]
        (if (empty? linksets)
          (swap! solutions conj (conj path w))
          (some #(when (= w (first %))
                  (dfs-old solutions (rest linksets) % path))
                (first linksets))))))
