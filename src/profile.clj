(ns profile
  (:require [utilities :as u]
            [solver1 :as s1]
            [solver2 :as s2]
            [solver3 :as s3]
            [solver4 :as s4]
            [generator2 :as g2]
            [generator3 :as g3]
            [taoensso.tufte :refer (defnp p profiled profile add-basic-println-handler!)]
            #_[clj-async-profiler.core :as flame]

            [utilities :as utils]))

(def input [[4 \P] [2 0] [1 \N] [3 4] [0 \F] [1 2] [3 \C] ])
(def classic [[4 \P] [2 0] [1 \N] [3 4] [0 \F] [1 2] [3 \C] [0 4] [2 \A]])
(def bad [[4 \Q] [2 0] [1 \Z] [3 4] [0 \F] [1 2] [3 \U]])

(def ? #(do (println %) %))
;; (def puzzle (g3/generate 5))
;; puzzle


;; (flame/profile (dotimes [_ 5] (solutions input utils/all-words)))
;; (flame/serve-ui 8080)
(add-basic-println-handler! {})
(profile
 {}
   (dotimes [_ 30]
     #_(p :s3-old        (s3/solutions input u/all-words))
     #_(p :s4-accept     (s4/solutions input u/all-words))
     #_(p :s4-reject     (s4/solutions bad u/core-words))
     ))
