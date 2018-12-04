(ns triangle-game.core
  (:require [clojure.contrib.string :as string])
  (:gen-class))

(def result-rankings '("" "You're genius" "You're purty smart" "You're just plain dump" "You're just plain 'eg-noor-a-moose'"))


(defn get-peg "gets a peg or space" [is-peg]
  (if is-peg 0 1))

(defn split-tree "takes a list and builds it into the game board format"
  ([point l]
   (split-tree point l '() )
    )
  ([point l result]
   ( if (empty? l)
     (reverse result)
     (split-tree ( + point 1) (last (split-at point l)) (conj result (first (split-at point l))))
    )
  ))

(defn start-game []
  (def space (rand-int 15))
  (def game (map #(get-peg (= % space)) (range 15)))
  (split-tree 1 game )
  )

(defn print-game-line "takes a list and prints out a line of the game"
  [line]
  (str (clojure.string/join "" (take (- 5 (count line)) (repeat " "))) (clojure.string/join " " line)))

(defn print-game "print out a representation of the game" [game]
  (doseq [x game] (println (print-game-line x))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (def game (start-game))
  (print-game game))
