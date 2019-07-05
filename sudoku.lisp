(defun nth-dim (matrix &rest indices)
  (let* ((index (if (listp (first indices))
                    (first indices)
                    indices))
         (result (nth (first index) matrix)))
    (if (listp result)
        (nth-dim result (rest index))
        result)))

(defun consistent-list (l)
  (if (or (null l) (= (list-length l) 1))
      t
      (if (eql (first l) (second l))
          (consistent-list (rest l))
          nil)))


(defparameter primes '(2 3 5 7 11 13 17 19 23))
(defparameter numbers '(1 2 3 4 5 6 7 8 9))
(defparameter prime-product (apply #'* primes))

(defparameter test-input '((0 8 3 1 0 4 7 0 5)
                           (2 0 6 0 0 9 0 0 1)
                           (0 4 0 5 3 0 6 0 0)
                           (0 0 0 0 0 7 1 9 2)
                           (1 9 0 0 0 0 3 7 0)
                           (3 2 0 9 0 1 0 0 4)
                           (0 1 0 7 0 0 9 0 0)
                           (8 0 5 4 9 6 0 1 7)
                           (4 7 9 0 0 0 8 5 6)))
(defparameter test2 '((9 0 0 0 0 0 0 0 0)
                      (1 0 5 0 6 0 2 3 0)
                      (0 8 0 3 4 5 0 0 7)
                      (0 9 3 0 0 6 0 0 0)
                      (0 0 1 0 0 0 3 0 0)
                      (0 0 0 1 0 0 6 7 0)
                      (3 0 0 7 5 2 0 6 0)
                      (0 5 9 0 1 0 7 0 8)
                      (0 0 0 0 0 0 0 0 3)))

(defun 2d-iterator (matrix func)
  (let ((rows (list-length matrix))
        (columns (list-length (first matrix))))
    (loop for r below rows
          collect (loop for c below columns
                        collect (funcall func r c)))))

(defun primep (n) ; Returns n if prime, not t
  (member n primes))

(defun number-to-prime (n)
  (if (zerop n)
      n
      (nth (1- n) primes)))

(defun prime-to-number (n)
  (if (zerop n)
      n
      (let ((mapping (position n primes)))
        (if (null mapping)
            (error "Value not in primes")
            (1+ mapping)))))

(defun map-to-prime (sudoku)
  (2d-iterator sudoku
               (lambda (r c)
                 (number-to-prime (nth-dim sudoku (list r c))))))

(defun map-from-prime (sudoku)
  (2d-iterator sudoku
               (lambda (r c)
                 (prime-to-number (nth-dim sudoku (list r c))))))

(defun box-coords-to-grid (outer inner)
  (list (+ (* 3 (truncate outer 3))
           (truncate inner 3))
        (+ (* 3 (mod outer 3))
           (mod inner 3))))

(defun resolve-box (r c)
  (+ (* 3 (truncate r 3)) (truncate c 3)))

(defun possibility-product (set)
  (let ((filtered-set (remove-if-not #'primep set )))
    (if (null filtered-set)
        prime-product
        (/ prime-product (apply #'* filtered-set)))))

(defun index-of-factor (factor number &optional (count 0))
  (multiple-value-bind (division remainder) (truncate number factor)
    (if (zerop remainder)
        (index-of-factor factor division (1+ count))
        count)))

(defun unique-prime-product (set)
  (let ((nonprime-product (apply #'* (remove-if #'primep set)))
        (primes-preexisting (remove-if-not #'primep set)))
    (apply #'* (remove-if-not (lambda (x)
                                (and (= (index-of-factor x nonprime-product) 1)
                                     (not (member x primes-preexisting))))
                              primes))))

(defun combination-matrix (sudoku func)
  (labels ((transformer (transform)
             (mapcar func (2d-iterator sudoku transform))))
    (list (transformer (lambda (r c)
                         (nth-dim sudoku r c))) ; Rows
          (transformer (lambda (r c)
                         (nth-dim sudoku c r))) ; Columns
          (transformer (lambda (r c)
                         (nth-dim sudoku (box-coords-to-grid r c))))))) ; Boxes

(defun get-from-combination (combination r c)
  (list (nth-dim combination 0 r)
        (nth-dim combination 1 c)
        (nth-dim combination 2 (resolve-box r c))))

(defun replace-nonprime-with-possibilities (sudoku)
  (let ((possibility-matrix (combination-matrix sudoku #'possibility-product)))
    (2d-iterator sudoku
                 (lambda (r c)
                   (let ((value (nth-dim sudoku r c)))
                     (if (primep value)
                         value
                         (apply #'gcd
                                (get-from-combination possibility-matrix r c))))))))

(defun replace-with-unique-prime (sudoku)
  (labels ((find-substitutions (unique-primes value)
             (remove-if (lambda (x) (= x 1))
                        (mapcar (lambda (x) (gcd x value))
                                unique-primes))))
  (let ((unique-prime-matrix (combination-matrix sudoku #'unique-prime-product)))
    (2d-iterator sudoku
                 (lambda (r c)
                   (let* ((unique-primes (get-from-combination unique-prime-matrix r c))
                          (value (nth-dim sudoku r c))
                          (substitutions (find-substitutions unique-primes value)))
                     ; (format t "R/C: ~A/~A | Val: ~A |~%" r c value)
                     ; (format t "Unique: ~A | Subs: ~A|~% ------------ ~%" unique-primes substitutions)
                     (cond
                       ((null substitutions) value)
                       ((consistent-list substitutions) (first substitutions))
                       (t (error (format nil "Inconsistent sub list: ~A" substitutions))))))))))

(defun get-row (sudoku r c)
  (nth r sudoku))

(defun get-column (sudoku r c)
  (mapcar (lambda (row) (nth c row)) sudoku))

(defun get-box (sudoku r c)
  (let* ((box-number (resolve-box r c))
         (start (box-coords-to-grid box-number 0)
         (end   (box-coords-to-grid box-number 9))))
    (mapcar (lambda (row) (subseq row (second start) (second end))
            (subseq sudoku (first start) (first end))))))

(defun build-dependency-tree (possibility-matrix r c b)
  
  (let ((flat-tree '()))
    (labels ((value-at (r c) (gcd (nth r (first possiblity-matrix))
                                    (nth c (second possibility-matrix))
                                    (nth (resolve-box r c) (third possibility-matrix))))
             (scanner (r c b shared-primes)
               (let ((value (value-at r c b))
                     (flat-tree (append flat-tree (list (list r c b))))
                     (dependents (apply #'append
                                        (mapcar (lambda (p) (list r p (resolve-box r p)))
                                                (position-if (lambda (x) (gcd value (value-at r p (resolve-box r p))))
                                                             (second ))))
                 


'((23     23205 4641  57     969   646   2926 2926   2002)
  (2      119   11    437    13    7429  3    5      161)
  (39     19    39    5      7     11    46   46     17)
  (74613  23    5     4389   969   13    2926 8778   462)
  (969969 4641  2     100947 22287 52003 5    100947 5313)
  (4389   21    399   2      6555  15295 13   17     5313)
  (5      14    133   17     11    3     322  13     322)
  (273    11    23    91     2     35    17   21     19)
  (88179  9282  88179 39767  437   3059  3542 10626  5))
