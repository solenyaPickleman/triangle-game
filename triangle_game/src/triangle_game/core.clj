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

;play and solve
(defn which-row "given a peg index, return integer corresponding to row 1-4"
  [i]
  (cond
    (< i 1) 0
    (< i 3) 1
    (< i 6) 2
    (< i 10) 3
    :else 4)
  )
(defn which-element-in-row "given a peg index, return position in row"
  [i]
  (cond
    (< i 1) (- i 0)
    (< i 3) (- i 1)
    (< i 6) (- i 3)
    (< i 10) (- i 6)
       :else (- i 10)
    )
  )

(defn get-moves-samerow "given a hole, row, and game board, returns all moves on the same row. Moves are returned as possible game boards"
  [hole row game]
  (if (< row 2)
    '()
    (do
      (let [movegroups (partition 3 1 (nth game row))]
        (def potential-moves
          (filter #(reduce + %) (concat (filter #(= 0 (first %)) movegroups)
                                        (filter #(= 0 (last %)) movegroups))))
       ; (println potential-moves)
        ;replace row with possible future rows
        ;cast to string, substring replace with all possible options
        ( loop [ moves potential-moves
                 new_games () ]
          (let [ rowstring (clojure.string/join (nth game row))
                replace-with (clojure.string/join (map #(if (= 0 %) 1 0)  (first moves)))
                hole-num (which-element-in-row hole)
                ]
            (if (= (count moves) 0)
              new_games
              (recur (rest moves)
                   (conj new_games (conj
                     (take-last (- 4 row) game )
                     (reverse (into (list) (clojure.string/split  (str
                          (subs rowstring 0 (if (= 0 (first (first moves)))
                                              hole-num
                                              ( - hole-num 2  )))
                          replace-with
                          (subs rowstring (if (= 0 (first (first moves)))
                                            (do (+ hole-num 3))
                                            (do ( + 1 hole-num))))) #"")))
                     (take row game)))))
            ))))))

(defn get-moves-above "given a hole, row, and game board, returns all moves from above the row. Moves are returned as possible game boards"
  [hole row game]
  (if (= row 0)
    '()
    (do
      ;TODO write function that combines this and does the below ones as well... would save a lot of space. pass da function
      (def left-move (loop [
                            r row
                            index (which-element-in-row hole)
                            move '()]
                       (if (or (< row 0) (< index 0) (= (count move) 3))
                         (reverse move)
                         (recur (- row 1) (- index 1) (conj move (nth (nth game row) index))))
                       ))
      (def right-move (loop [
                            r row
                            index (which-element-in-row hole)
                            move '()]
                       (if (or (>= row (count game )) (>= index (count (nth game row))) (= (count move) 3))
                         (reverse move)
                         (recur (- row 1) (+ index 1) (conj move (nth (nth game row) index))))
                       ))
      (println left-move right-move)
      ;TODO check validity and return possible game states
      )
    ))


(defn get-moves-below "given a hole, row, and game board, returns all moves from below the row.  Moves are returned as possible game boards"
  [hole row game]
  '()
  )

(defn get-moves-for-hole "given a game board and specific hole, give all possible moves for that peg"
  [hole game]
  ;(def a (concat
  ;  (get-moves-above hole (which-row hole) game)
  ;  (get-moves-samerow hole (which-row hole) game)
  ;  (get-moves-below hole (which-row hole) game)
  ;  ))
  (println (get-moves-above hole (which-row hole) game))
  '(1 2 3)
  )

(defn get-moves "given a game board, gives all possible moves in the form of the potential game board"
  [game]
  (loop [index 0 moves ()]
                  (if (= index (- (count (flatten game)) 1))
                    moves ;at this point ,all have been looped through so return the vector of potential game boards
                    (recur
                      (+ index 1)
                      (if (= (nth (flatten game) index) 1)
                        moves
                        (concat moves (get-moves-for-hole index game))
                        )))))

;main
(defn -main
  "Build a random board of Cracker Barrel peg solitaire and solve that board"
  [& args]
  (def game (start-game))
  (println "Initial board" )
  (print-game game)
  (get-moves game)
  ;(print-game '((1) (1 1) (1 1 1) (1 1 1 1) (1 1 0 1 1)))
  ;(get-moves '((1) (1 1) (1 1 1) (1 1 1 1) (1 1 0 1 1)))
  )


