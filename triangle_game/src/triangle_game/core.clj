(ns triangle-game.core
  (:require [clojure.contrib.string :as string])
  (:gen-class))

(def result-rankings '("" "You're genius" "You're purty smart" "You're just plain dump" "You're just plain 'eg-noor-a-moose'"))

;this section builds a  game
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

(defn which-element-in-game "given row and index, return overall index of peg in game"
  [row index]
  ( + index (cond
              (= row 0) 0
              (= row 1) 1
              (= row 2) 3
              (= row 3) 6
              :else 10
              ))
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
              (map #(split-tree 1 (flatten %)) new_games)
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
  (if (< row 2)
    '() ; there are no moves in the first or second rows
    (do
      ;TODO write function that combines this and does the below ones as well... would save a lot of space. pass da function
      (def left-move (loop [
                            r row
                            index (which-element-in-row hole)
                            move '()]
                       (if (or (< r 0) (< index 0) (= (count move) 3))
                         (reverse move)
                         (recur (- r 1) (- index 1) (conj move (which-element-in-game r index))))
                       ))
      (def right-move (loop [
                            r row
                            index (which-element-in-row hole)
                            move '()]   ;TODO fix r = -1 error
                       (if (or (>= r (count game )) (>= index (count (nth game r))) (= (count move) 3))
                         (reverse move)
                         (recur (- r 1)  index (conj move (which-element-in-game r index))))
                       ))
      (def movelist (filter #(= 3 (count %)) (list right-move left-move)))
      (loop [ moves (filter #(= 2 (reduce +
                                          (let [g (flatten game)]
                                            (list
                                              (nth g (first %))
                                              (nth g (second %))
                                              (nth g (nth % 2))))))
                            movelist )
             new_games () ]
        (if ( = (count moves) 0)
          new_games
          (recur (rest moves) (conj new_games (split-tree 1 (apply list
                                                                   (assoc
                                                                     (assoc  (assoc (vec (flatten game)) (first (first moves)) 1) (second (first moves)) 0)
                                                                     (nth (first moves) 2) 0)
                                                                   ))))))
      )
    ))

(defn get-moves-below "given a hole, row, and game board, returns all moves from below the row.  Moves are returned as possible game boards"
  [hole row game]
  (if (> row 2)
    '() ; there are no moves in the last 2 rows going down.
    (do
      ;TODO write function that combines this and does the above ones as well... would save a lot of space. pass da function
      (def left-move (loop [
                            r row
                            index (which-element-in-row hole)
                            move '()]
                       (if (or (< r 0) (< index 0) (= (count move) 3))
                         (reverse move)
                         (recur (+ r 1) index (conj move (which-element-in-game r index))))
                       ))
      (def right-move (loop [
                             r row
                             index (which-element-in-row hole)
                             move '()]
                        (if (or (>= r (count game )) (>= index (count (nth game r))) (= (count move) 3))
                          (reverse move)
                          (recur (+ r 1) (+ index 1) (conj move (which-element-in-game r index))))
                        ))
      (def movelist (filter #(= 3 (count %)) (list right-move left-move)))
      (loop [ moves (filter #(= 2 (reduce +
                                          (let [g (flatten game)]
                                            (list
                                              (nth g (first %))
                                              (nth g (second %))
                                              (nth g (nth % 2))))))
                                          movelist)
              new_games () ]
        (if ( = (count moves) 0)
          new_games
          (recur (rest moves) (conj new_games (split-tree 1 (apply list
                                                (assoc
                                                  (assoc  (assoc (vec (flatten game)) (first (first moves)) 1) (second (first moves)) 0)
                                                  (nth (first moves) 2) 0)
                                                ))))))
      )))

(defn get-moves-for-hole "given a game board and specific hole, give all possible moves for that peg"
  [hole game]
  (concat
    (get-moves-above hole (which-row hole) game)
    (get-moves-samerow hole (which-row hole) game)
    (get-moves-below hole (which-row hole) game)
  ))

(defn get-moves "given a game board, gives all possible moves in the form of the potential game board"
  [game]
  (println "get-moves" game)
  (loop [index 0
         moves ()]
                  (if (= index (count (flatten game)) )
                    moves ;at this point ,all have been looped through so return the vector of potential game boards
                    (recur
                      (+ index 1)
                      (if (= (nth (flatten game) index) 1)
                        moves
                        (concat moves (get-moves-for-hole index game))
                        )))))

;main
(defn make-int "ensures all elements in list are int"
  [l]
  (map #(if (= java.lang.String (type %)) (read-string %) %) l))

(defn -main
  "Build a random board of Cracker Barrel peg solitaire and solve that board"
  [& args]
  (def game (start-game))
  (println "Initial board" )
  (print-game game)
  ;TODO - write bit that recurs through games
  ;TODO - upper right not working
  (loop [games (get-moves game)]
    (if (< 0 (count (filter #(= 1 %)
                       (map #(reduce + (make-int (flatten %))) games))))
      (println "done" (filter #(= 1 %)(map #(reduce + (make-int (flatten %))) games)))
      (do
        (println (map #(reduce + (make-int (flatten %))) games))
        (recur (reduce list (map #(get-moves %) games))))
        )
      )
    )

  ;(println (map #(reduce + (flatten %)) t))





