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

(defconstant zero-bits #*000000000)

(defun map-rec (func l)
  (if (atom (car l))
      (mapcar func l)
      (mapcar (lambda (x) (map-rec func x)) l)))

(defun nth-dim (matrix &rest indices)
  (let* ((index (if (listp (first indices))
                    (first indices)
                    indices))
         (result (nth (first index) matrix)))
    (if (listp result)
        (nth-dim result (rest index))
        result)))

(defun int-power->bit-vector (n)
  (declare ((integer 0 9) n))
  (if (zerop n)
      #*000000000
      (concatenate 'bit-vector
               (make-array (- 9 n)
                           :element-type 'bit
                           :initial-element 0)
               #*1
               (make-array (- n 1)
                           :element-type 'bit
                           :initial-element 0))))

(defun numbers->bits (sudoku)
  (map-rec #'int-power->bit-vector sudoku))

(defun bit-columns (set)
  (apply #'map 'list
         (lambda (&rest bits)
           (make-array (list-length bits)
                       :element-type 'bit
                       :initial-contents bits))
         set))

(defun 2d-iterator (matrix func)
  (let ((rows (list-length matrix))
        (columns (list-length (first matrix))))
    (loop for r below rows
          collect (loop for c below columns
                        collect (funcall func r c)))))

(defmacro 2d-collector ((r c value) matrix &body body)
  `(declare (ignorable value))
  `(let ((rows (list-length ,matrix))
        (columns (list-length (first ,matrix))))
    (loop for ,r below rows
          collect (loop for ,c below columns
                        collect (let ((,value (nth-dim ,matrix ,r ,c)))
                                  ,@body)))))

(defun box-coords-to-grid (outer inner)
  (list (+ (* 3 (truncate outer 3))
           (truncate inner 3))
        (+ (* 3 (mod outer 3))
           (mod inner 3))))

(defun resolve-box (r c)
  (+ (* 3 (truncate r 3)) (truncate c 3)))

(defun int-power-p (vec)
  (= 1 (count 1 vec)))

(defun possibilities (set)
  (bit-not (reduce #'bit-ior set)))

(defun unique-numbers (set)
  (reduce #'bit-ior
          (append (list #*000000000) ; Guard against no unique values
                  (remove-if-not #'int-power-p
                                 (bit-columns (substitute-if zero-bits
                                                             #'int-power-p
                                                             set))))))

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

(defun superimpose-possibilities (sudoku)
  (let ((possibility-matrix (combination-matrix sudoku #'possibilities)))
    (2d-collector (r c value) sudoku
      (if (int-power-p value)
          value
          (reduce #'bit-and (get-from-combination possibility-matrix r c))))))

(defun replace-with-unique (sudoku)
  (let ((unique-matrix (combination-matrix sudoku #'unique-numbers)))
    (2d-collector (r c value) sudoku
      (if (int-power-p value)
          value
          (bit-and value (reduce #'bit-ior (get-from-combination unique-matrix r c)))))))

(defun incomplete-cells (sudoku)
  (remove-if #'null
             (apply #'append
                    (2d-collector (r c value) sudoku
                      (unless (int-power-p value)
                        (list r c value))))))

(defun dependent-cell-p (parent child exclude)
                  (and (not (equal #*000000000 (bit-and (third parent)
                                                        (third child)))) ; Numbers in common.
                       (or (equal (first parent) (first child))            ; In the same row,
                           (equal (second parent) (second child))          ; column,
                           (equal (resolve-box (first parent) (second parent))
                                  (resolve-box (first child) (second child)))) ; or box.
                       (not (member child exclude)))) ; Not in the exclusion list

(defun adjacency-matrix (cell-list)
  ; Given the list of incomplete cells, returns a matrix of all "adjacent" cells that fulfil the dependent-cell-p criteria.
  (mapcar (lambda (cell)
            (map 'bit-vector
                 (lambda (counterpart)
                   (if (dependent-cell-p cell counterpart (list cell))
                                              1
                                              0))
                    cell-list))
          cell-list))

(defun once-removed (row matrix)
  ; For the row of the adjacency matrix, returns the row of cells that are one step removed, or "two walks".
  (labels ((mask-column (c) (bit-and row c))
           (remove-only-in-latter (c) (bit-and (bit-not row) c)))
    (reduce #'bit-ior
            (mapcar #'remove-only-in-latter
                    (bit-columns (mapcar #'mask-column
                                         matrix))))))

(defun adjacent-adjacent (matrix)
  (mapcar (lambda (row) (once-removed row matrix)) matrix))


