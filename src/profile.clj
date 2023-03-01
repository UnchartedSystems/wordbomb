(ns profile
  (:require [utilities :as utils]
            [solver1 :as s1]
            [solver2 :as s2]
            [solver3 :as s3]
            [solver4 :as s4]
            [taoensso.tufte :refer (defnp p profiled profile add-basic-println-handler!)]
            #_[clj-async-profiler.core :as flame]
            ))


(def input [[4 \P] [2 0] [1 \N] [3 4] [0 \F] [1 2] [3 \C]])

;; (flame/profile (dotimes [_ 5] (solutions input utils/all-words)))
;; (flame/serve-ui 8080)

(add-basic-println-handler! {})
(profile
 {}
   (dotimes [_ 30]
     (p :4a (solver4/solutions input utils/all-words))
     ))
