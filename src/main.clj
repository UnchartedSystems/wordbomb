(ns main
  (:require [player2 :as p]
            [text :as t]))

(defn- main [& cli-args]
  (player2/game)
  #_(text/test-noop))
