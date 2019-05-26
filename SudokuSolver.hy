#! /usr/bin/env hy

                                ; TODO Make narrow board handle boxes
                                ; Integrate narrow-board into the solving loop


(import [hy.extra.anaphoric [x]])
(import [numpy :as np])
(import [sudokutools.generate [generate]])

(defn product [x y] (* x y))
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

(defn possibility-product [set]
  "Returns the product of all the primes not in the set"
  (setv filtered-set (list (ap-filter (in x primes) set)))
  (if (any filtered-set)
      (// prime-product
          (* (unpack-iterable filtered-set)))
      prime-product))



(defn get-boxes [board]
  (np.reshape (np.array (list (map (fn [row] (np.hsplit row 3))
                                    (np.vsplit board 3))))
              '(9 9)))

(defn resolve-box [r c]
  (+ (* 3 (// r 3)) (// c 3)))

(defn shared-prime-factor [set]
  (np.gcd.reduce
    (list (ap-reject (in x primes) set))))

(defn possibilities [sets]
  (np.array (list (map possibility-product sets))))

(defn combination-matrix [board function]
  {:rows (function board)
   :columns (function (np.transpose board))
   :boxes (function (get-boxes board))})

(defn get-from-combination [combination-matrix r c]
  [(get (:rows combination-matrix) r)
   (get (:columns combination-matrix) c)
   (get (:boxes combination-matrix) (resolve-box r c))])

(defn nonprime-processor [board matrix-generator processor-function]
  (setv matrix (combination-matrix board matrix-generator))
  (setv board (np.copy board))
  (for [(, r c) (np.ndindex (np.shape board))]
    (if (not (in (get board r c) primes))
        (setv (get board r c) (processor-function matrix r c))))
  board)

(defn replace-nonprime-with-possibilities [board]
  (nonprime-processor possibilities
                      (fn [matrix r c]
                        (np.gcd.reduce (get-from-combination matrix r c)))))

; Not sure if the narrowing algorithm is actually compatible with nonprime-processor
(defn narrow-board [board]
  (nonprime-processor shared-prime-factor
                      (fn [matrix r c]
                        (setv shared (get-from-combination matrix r c))

    

(defn narrow-set [set]
  (setv gcp (np.gcd.reduce
              (list (filter (fn [x]
                              (not (in x primes)))
                            set))))
  (list (ap-map
               (if (in x primes)
                   x
                   (if (in (// x gcp) primes)
                       (// x gcp)
                       x))
             set)))

(defn narrow-board [board]
  (setv rows-narrowed (np.array (list (map narrow-set board))))
  (setv columns-narrowed (np.transpose
                           (np.array
                             (list
                               (map narrow-set (np.transpose rows-narrowed))))))
  (print "In narrow-board: " rows-narrowed " Done ")
  columns-narrowed)

(defn solver [board]
  (setv previous (np.zeros (np.shape board)))
  (setv counter 0)
  (while (not (np.array_equal previous board))
    (setv counter (+ 1 counter))
    (print counter)
    (setv previous board)
    (setv board (narrow-board (replace-nonprime-with-possibilities board))))
  (if (any (list (ap-reject (in x primes) (.flatten board))))
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

