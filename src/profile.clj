(ns profile
  (:require [utilities :as utils]
            [solver :as s1]
            [solver2 :as s2]
            [solver4 :as s4]
            [taoensso.tufte :refer (defnp p profiled profile add-basic-println-handler!)]
            #_[clj-async-profiler.core :as flame]
            ))


(def input [[4 \P] [2 0] [1 \N] [3 4] [0 \F] [1 2] [3 \C]])


;; (flame/profile (dotimes [_ 5] (solutions input utils/all-words)))
;; (flame/serve-ui 8080)


#_(profile {}
         (dotimes [_ 100]
         (p :merge (merge '() '()))
         (p :big-merge (merge '((1 2 3 4 (1 2 3 4 5 6 7 8 9 10) 5 6 7 (1 2 3 4 5 6 7 8 9 10) 8 9 10)) '((1 2 3 4 (1 2 3 4 (1 2 3 4 5 6 7 8 9 10) 5 6 7 (1 2 3 4 5 6 7 8 9 10) 8 9 10) 5 6 7 8 9 10) 5 6 7 (1 2 3 4 5 6 7 8 9 10) 8 9 10)))
         (p :empty? (empty? '(((1 2 3 4 (1 2 3 4 5 6 7 8 9 10) 5 6 7 (1 2 3 4 5 6 7 8 9 10) 8 9 10)) '((1 2 3 4 (1 2 3 4 (1 2 3 4 5 6 7 8 9 10) 5 6 7 (1 2 3 4 5 6 7 8 9 10) 8 9 10) 5 6 7 8 9 10) 5 6 7 (1 2 3 4 5 6 7 8 9 10) 8 9 10))))
         (p :map (map #(println %) '()))
         (p :count (count '(((1 2 3 4 (1 2 3 4 5 6 7 8 9 10) 5 6 7 (1 2 3 4 5 6 7 8 9 10) 8 9 10)) '((1 2 3 4 (1 2 3 4 (1 2 3 4 5 6 7 8 9 10) 5 6 7 (1 2 3 4 5 6 7 8 9 10) 8 9 10) 5 6 7 8 9 10) 5 6 7 (1 2 3 4 5 6 7 8 9 10) 8 9 10))))
         (p :flat (flatten '("1" (1 2 3 4 (1 2 3 4 5 6 7 8 9 10) 5 6 7 (1 2 3 4 5 6 7 8 9 10) 8 9 10) 2 3 4 (1 2 3 4 (1 2 3 4 (1 2 3 4 5 6 7 8 9 10) 5 6 7 (1 2 3 4 5 6 7 8 9 10) 8 9 10) 5 6 7 8 9 10) 5 6 7 (1 2 3 4 5 6 7 8 9 10) 8 9 10)))
         (p :apply (apply = true '(true true true true false false true true)))
         (p :every? (every? true? '(true true true true false false true true)))
         (p :jva (.charAt "HELLO" 3))
         (p :get (get "HELLO" 3))
         (p :nth (nth "HELLO" 3))
         (p :reduce (reduce + 0 '(1 2 3 4 5 6 7 8 9 10)))))


(add-basic-println-handler! {})
(profile
 {}
 (let [rows    (take-nth 2 input)]
   (dotimes [_ 3]
     (p :4d (s4/linksets4 input utils/all-words))
     (p :4c (s4/linksets3 input utils/all-words))
     (p :4b (s4/linksets2 input utils/all-words))
     (p :4a (s4/linksets input utils/all-words))
     ;; (p :ks (keep seq '(() (1) (1 2) () (3))))
     ;; (p :ke (keep not-empty '(() (1) (1 2) () (3))))
     ;; (p :fe (filter not-empty '(() (1) (1 2) () (3))))
     ;; (p :fv (filterv not-empty '(() (1) (1 2) () (3))))
     )))
