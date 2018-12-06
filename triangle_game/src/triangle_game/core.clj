(ns triangle-game.core
  (:require [clojure.contrib.string :as string])
  (:gen-class))

(def result-rankings '("" "You're genius" "You're purty smart" "You're just plain dump" "You're just plain 'eg-noor-a-moose'"))

;build game

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

;display game
(defn print-game-line "takes a list and prints out a line of the game"
  [line]
  (str (clojure.string/join "" (take (- 5 (count line)) (repeat " "))) (clojure.string/join " " line)))

(defn print-game "print out a representation of the game" [game]
  (doseq [x game] (println (print-game-line x))))

(defn get-moves-for-hole "given a game board and specific hole, give all possible moves for that peg"
  [hole game]
  (println hole)
  '(1 2 3)
  )
;play and solve game
(defn get-moves "given a game board, gives all possible moves in the form of the potential game board"
  [game]

  (def testaroo (loop [index 0 moves ()]
                  (if (= index (- (count (flatten game)) 1))
                    moves ;at this point ,all have been looped through so return the vector of potential game boards
                    (recur
                      (+ index 1)
                      (if (= (nth (flatten game) index) 1)
                        moves
                        (concat moves (get-moves-for-hole index game))
                        )))
                  )
    )
  (println testaroo)
  )

 (defn which-row "given a peg index, return integer corresponding to row 1-4"
    [i]
    (cond
      (< i 1) 0
      (< i 3) 1
      (< i 6) 2
      (< i 10) 3
      :else 4)
    )
(defn get-surrounding "given a hole, get surrounding pegs #thats what she said"
  [hole game]
  )

;main
(defn -main
  "Build a random board of Cracker Barrel peg solitaire and solve that board"
  [& args]
  (def game (start-game))
  (println "Initial board" )
  (print-game game)
  (get-moves game)
  )


