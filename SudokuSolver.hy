#! /usr/bin/env hy

                                ; Criticisms of Hylang: itertools and lists should have implicit conversion
; Optional parameters should have default values ffs

(require [hy.extra.anaphoric [*]])
(import [numpy :as np])
(import [sudokutools.generate [generate]])


(defmacro np-map [function input]
  `(np.array (list (map ~function ~input))))

(defmacro ap-np-map [function input]
  `(np.array (list (ap-map ~function ~input))))

; TODO Allow docstrings
(defmacro defvec [name params body]
  `(defun ~name ~params ~@body)
  `(setv ~name (np.vectorize ~name)))

(setv primes [2 3 5 7 11 13 17 19 23])
(setv numbers [1 2 3 4 5 6 7 8 9])
(setv prime-product (* (unpack-iterable primes)))
(setv test-input (np.array [[0 8 3 1 0 4 7 0 5]
                  [2 0 6 0 0 9 0 0 1]
                  [0 4 0 5 3 0 6 0 0]
                  [0 0 0 0 0 7 1 9 2]
                  [1 9 0 0 0 0 3 7 0]
                  [3 2 0 9 0 1 0 0 4]
                  [0 1 0 7 0 0 9 0 0]
                  [8 0 5 4 9 6 0 1 7]
                  [4 7 9 0 0 0 8 5 6]]))
(setv test2 (np.array [
                       [9 0 0 0 0 0 0 0 0]
                       [1 0 5 0 6 0 2 3 0]
                       [0 8 0 3 4 5 0 0 7]
                       [0 9 3 0 0 6 0 0 0]
                       [0 0 1 0 0 0 3 0 0]
                       [0 0 0 1 0 0 6 7 0]
                       [3 0 0 7 5 2 0 6 0]
                       [0 5 9 0 1 0 7 0 8]
                       [0 0 0 0 0 0 0 0 3]]))

(setv map-to-prime (np.vectorize (fn [x]
  (if (= x 0)
      0
      (get (dict (zip numbers primes)) x)))))


(setv map-from-prime
      (np.vectorize
        (fn [x]
          (if (in x primes)
              (get (dict (zip primes numbers)) x)
              x))))

(defn lister [l]
  (list (.tolist (np.array l))))

(defn list-product [factors &optional acc]
  (setv acc (if acc acc 1))
  (setv factors (lister factors))
  (if (empty? factors)
      acc
      (list-product (list (rest factors)) (* (first factors) acc))))

(defn get-boxes [board]
  (np.reshape (np-map (fn [row] (np.hsplit row 3)) (np.vsplit board 3))
              '(9 9)))

(defn resolve-box [r c]
  (+ (* 3 (// r 3)) (// c 3)))

(defn possibility-product [set]
  "Returns the product of all the primes not in the set"
  (setv filtered-set (lister (ap-filter (in it primes) set)))
  (if (any filtered-set)
      (// prime-product
          (* (unpack-iterable filtered-set)))
      prime-product))

(defn shared-prime-factor [set]
  "Returns the common factor, a product of primes, of all the non-primes in the set"
  (np.gcd.reduce
    (list (ap-reject (in it primes) set))))

(defn index-of-factor [factor number &optional [count 0]]
  "Computes the the repetitions of a factor in a product"
  (setv result (divmod number factor))
  (if (zero? (second result))
      (index-of-factor factor (first result) (inc count))
      count))

(defn unique-prime-product [set]
  "Returns the product of all the primes that only occur once in the set"
  (setv nonprime-product (list-product (ap-reject (in it primes) set)))
  (list-product
       (ap-filter (= (index-of-factor it nonprime-product) 1)
                  primes)))

(defn combination-matrix [board function]
  "The values of a function applied to each of the rows, columns and boxes of the board"
  {:rows (np-map function board)
   :columns (np-map function (np.transpose board))
   :boxes (np-map function (get-boxes board))})

(defn get-from-combination [combination-matrix r c]
  "Return a list of the combination values for the row, column and box to which a square belongs."
  [(get (:rows combination-matrix) r)
   (get (:columns combination-matrix) c)
   (get (:boxes combination-matrix) (resolve-box r c))])

(defn nonprime-processor [board processor-function]
  "Applies a function to every non-prime number in the grid"
  (setv board (np.copy board))
  (setv dimensions  (np.ndindex (np.shape board)))
  (for [(, r c) dimensions]
    (if (not (in (get board r c) primes))
        (setv (get board r c) (processor-function r c))))
  board)

(defn replace-nonprime-with-possibilities [board]
  (setv possibility-matrix (combination-matrix board possibility-product))
  (nonprime-processor board
                      (fn [r c]
                        (np.gcd.reduce (get-from-combination possibility-matrix r c)))))

; Use a possibility matrix to find whether the suggested substitution is a possibility - or maybe a duplicate matrix?
(defn narrow-board [board]
  (setv unique-prime-matrix (combination-matrix board unique-prime-product))
  (nonprime-processor
    board
    (fn [r c]
      (setv unique-primes (get-from-combination unique-prime-matrix r c)
            value  (get board r c) 
            substitutions (list (ap-reject (= it 1)
                                     (ap-map (np.gcd it value)
                                             unique-primes))))
      (print "----------")
      (print "Row: " r " Column: " c " Value: " value)
      (print "Unique primes: " unique-primes " Substitutions: " substitutions)
      (if (empty? substitutions)
          value
          (first substitutions))))) ; Subsitutions should contain one unique value at this point. TODO Add a check for this also somehow getting ones


      

(defn solver [board]
  (setv previous (np.zeros (np.shape board)))
  (setv counter 0)
  (while (not (np.array_equal previous board))
    (setv counter (+ 1 counter))
    (print counter)
    (setv previous board)
    (setv board (replace-nonprime-with-possibilities board))
    (setv board (narrow-board board)))
  (if (any (list (ap-reject (in it primes) (.flatten board))))
      (print "Stuck!")
      (print "Solved!"))
  board)

(defn sudoku-solver [input]
  (print input)
  (-> input (map-to-prime) (solver) (map-from-prime)))

(defn random-sudoku []
  (np.reshape (np.fromstring (.encode (generate )
                                      " " " ")
                             :dtype int
                             :sep " ")
              '(9 9)))

(defmain [&rest args]
  (sudoku-solver test2))

