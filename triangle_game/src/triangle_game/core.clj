(ns triangle-game.core
  (:require [clojure.contrib.string :as string])
  (:gen-class))

(def result-rankings '("" "You're genius" "You're purty smart" "You're just plain dump" "You're just plain 'eg-noor-a-moose'"))

;this section builds a  game
(defn get-peg "gets a peg or space" [is-peg]
  (if is-peg 0 1))

(defn make-int "ensures all elements in list are int"
  [l]
  (into '[] (map #(if (= java.lang.String (type %)) (read-string %) %) l)))

(defn split-tree "takes a list and builds it into the game board format"
  ([point l]
   (split-tree point l '())
    )
  ([point l result]
   (if (empty? l)
     (into [] (reverse result))
     (split-tree (+ point 1) (subvec l point) (conj result (subvec l 0 point)))
     )
    ))

(defn start-game "initializes a random initial game board"
  ([]
   (start-game (rand-int 15)))
  ([space]
   (def game (vec (map #(get-peg (= % space)) (range 15))))
   (split-tree 1 game))
  )


;display game
(defn print-game-line "takes a list and prints out a line of the game"
  [line]
  (str (clojure.string/join "" (take (- 5 (count line)) (repeat " "))) (clojure.string/join " " line)))

(defn print-game "print out a representation of the game" [game]
  (println "print-game point" game)
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
  (+ index (cond
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
    '[]
    (do
      (def left-move
        (if (< (- (which-element-in-row hole) 2) 0)
          '[]
          (subvec (nth game row) (- (which-element-in-row hole) 2) (+ 1 (which-element-in-row hole)))))
      (def right-move
        (if (> (+ 2 (which-element-in-row hole)) row)       ;if the position is greater then the index of row, it is out of bounds
          '[]
          (subvec (nth game row) (which-element-in-row hole) (+ 3 (which-element-in-row hole)))))
      (def movelist (map #(and (= 2 (reduce + %)) (= 3 (count %))) (list left-move right-move)))
      ;replace row with possible future rows
      ;cast to string, substring replace with all possible options
      (loop [moves movelist left true
             new_games []]
        (if (empty? moves)
          (into [] (filter #(not (nil? %)) new_games))
          (recur (into [] (rest moves)) false (conj new_games
                                                    (if (not (first moves)) nil
                                                                            (split-tree 1 (into [] (flatten (conj (subvec game 0 row)
                                                                                                                  (let [r (nth game row)
                                                                                                                        start (if left (- (which-element-in-row hole) 2) (which-element-in-row hole))
                                                                                                                        end (if left (which-element-in-row hole) (+ 3 (which-element-in-row hole)))]
                                                                                                                    (into [] (flatten (conj (subvec r 0 start) (into [] (map #(if (zero? %) 1 0) (subvec r start end))) (subvec r end)))))                                                       (subvec game (+ row 1))))))))))))))


(defn get-moves-above "given a hole, row, and game board, returns all moves from above the row. Moves are returned as possible game boards"
  [hole row game]
  (if (< row 2)
    '[]                                                     ; there are no moves in the first or second rows
    (do
      ;TODO write function that combines this and does the below ones as well... would save a lot of space. pass da function
      (def left-move (loop [
                            r row
                            index (which-element-in-row hole)
                            move '[]]
                       (if (or (< r 0) (< index 0) (= (count move) 3))
                         move
                         (recur (- r 1) (- index 1) (conj move (which-element-in-game r index))))
                       ))
      (def right-move (loop [
                             r row
                             index (which-element-in-row hole)
                             move '[]]
                        (if (or
                              (< r 0)
                              (>= r (count game))
                              (>= index (count (nth game r)))
                              (= (count move) 3))
                          move
                          (recur (- r 1) index (conj move (which-element-in-game r index))))
                        ))
      (def movelist (filter #(= 3 (count %)) (list right-move left-move)))
      (loop [moves (filter #(= 2 (reduce +
                                         (let [g (make-int (flatten game))]
                                           (list
                                             (nth g (first %))
                                             (nth g (second %))
                                             (nth g (nth % 2))))))
                           movelist)
             new_games []]
        (if (= (count moves) 0)
          new_games
          (recur (rest moves) (conj new_games (split-tree 1 (into []
                                                                  (assoc
                                                                    (assoc (assoc (vec (flatten game)) (first (first moves)) 1) (second (first moves)) 0)
                                                                    (nth (first moves) 2) 0)
                                                                  ))))))
      )
    ))

(defn get-moves-below "given a hole, row, and game board, returns all moves from below the row.  Moves are returned as possible game boards"
  [hole row game]
  (if (> row 2)
    '[]                                                     ; there are no moves in the last 2 rows going down.
    (do
      ;TODO write function that combines this and does the above ones as well... would save a lot of space. pass da function
      (def left-move (loop [
                            r row
                            index (which-element-in-row hole)
                            move '[]]
                       (if (or (< r 0) (< index 0) (= (count move) 3))
                         move
                         (recur (+ r 1) index (conj move (which-element-in-game r index))))
                       ))
      (def right-move (loop [
                             r row
                             index (which-element-in-row hole)
                             move '[]]
                        (if (or (< r 0)
                                (>= r (count game))
                                (>= index (count (nth game r)))
                                (= (count move) 3))
                          move
                          (recur (+ r 1) (+ index 1) (conj move (which-element-in-game r index))))
                        ))
      (def movelist (filter #(= 3 (count %)) (list right-move left-move)))
      (loop [moves (filter #(= 2 (reduce +
                                         (let [g (make-int (flatten game))]
                                           (list
                                             (nth g (first %))
                                             (nth g (second %))
                                             (nth g (nth % 2))))))
                           movelist)
             new_games []]
        (if (= (count moves) 0)
          new_games
          (recur (rest moves) (conj new_games (split-tree 1 (into []
                                                                  (assoc
                                                                    (assoc (assoc (vec (flatten game)) (first (first moves)) 1) (second (first moves)) 0)
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
  (if ( = 15 (count game)) '[]
  (loop [index 0
         moves []]
    (if (= index (count (flatten game)))
      (vec moves)                                           ;at this point ,all have been looped through so return the vector of potential game boards
      (recur
        (+ index 1)
        (if (= (nth (flatten game) index) 1)
          moves
          (into [] (concat moves (get-moves-for-hole index game)))
          ))))))

;main
(defn -main
  "Build a random board of Cracker Barrel peg solitaire and solve that board"
  [& args]

  ;(loop [run 0]
  ;  (if (> run 14)
  ;    '[]
  ;    (do
  ;      (let [game (start-game run)]
  ;       (println "Initial board")
  ;       (print-game game)
  ;       (doall (map #(print-game %) (doall (get-moves game))))
  ;       )
  ;      (recur (+ run 1))
  ;    ))))

  (loop [games (start-game 3)
         first true
         sum 14]
    (if (and (not first) (some true? (map #(= 1 (reduce + (make-int (flatten %)))) games)))
      (println (into [] (filter #(= 1 (reduce + (make-int (flatten %)))) games)))
      (do
        (let [ testarino (into [] (set (filter #(<= sum (reduce + (make-int (flatten %)))) games)))]
          (recur
            (if first
              (get-moves games)
              (into [] (mapcat vec (map #(get-moves (make-int %)) testarino))))
            false
              (- sum 1))))))
  )






