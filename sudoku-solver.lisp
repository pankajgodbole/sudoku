(quicklisp:quickload "cl-ppcre")

(quicklisp:quickload "alexandria")

(defun my/range
    (max &key (min 0) (stp 1))
  "A range function to obtain a list of integers
Takes a parameter denoting the maximum value in the range (max), minimum value (min)
(optional) and a step (stp) (also optional), and returns a list of numbers"
  (loop :for n
	:from min
	  :to max
	:by stp
	:collecting n))

(defparameter *sdk/range-valid-indices*
  (my/range 8 :min 0)
  "The range of indices to denote the position of a cell in the Sudoku grid")

(defparameter *sdk/range-valid-values*
  (my/range 9 :min 1)
  "The range of values allowed in a cell in the Sudoku grid")

(defparameter *sdk/range-valid-values-and-zero*
  (my/range 9 :min 0)
  "The range of values allowed in a cell in the Sudoku grid, and zero (0)")

(defun my/utl/string-to-ints
    (s #| String of integers |#)
  "Converts a string of integers to a list of integers
   Takes a string (s) of numbers, converts it to a list of integers (l), and
   if all the elements of l are integers returns l, and signals an error
   otherwise
   Makes use of the regular-expression library 'cl-ppcre'"
  (mapcar #'parse-integer (cl-ppcre:split #\Space s)))

(defun my/utl/string-to-lists-ints
    (s #| String of integers |#)
  "Converts a string of integers to a list of list of integers
   Takes a string (s) of numbers, converts it to a list of list of integers
   (lli), and returns lli if all the elements of lli are integers, and
   signals an error otherwise
   Makes use of the regular-expression library 'cl-ppcre'"
  (mapcar #'list (mapcar #'parse-integer (cl-ppcre:split #\Space s))))

(defun sdk/read-row
    ()
  "Reads a string (s) of numbers, and returns a list of lists of integers if
   s is a valid Sudoku row and signals an error otherwise"
  (let* ((s (read-line))
	 (l (my/utl/string-to-ints s)))
    (unless (and (eql (length l) 9)
		 (every #'(lambda (x) (member x *sdk/range-valid-values-and-zero*)) l))
      (error "sdk/get-row: ERROR! Not a valid Sudoku row."))
    (mapcar #'list l)))

(defun sdk/read-grid
    ()
  "Returns a valid Suduku grid or signals an error
  A grid (g) is considered to be a valid Sudoku grid if -
    . g comprises 9 rows
    . each row in g is a valid Sudoku row"
  (make-array
   '(9 9)
   :initial-contents
   (loop
     :repeat 9
     :collecting (sdk/read-row))))

(defparameter *sdk/grid*
  #2A(((0) (0) (3) (7) (0) (5) (2) (0) (8))
      ((0) (0) (0) (0) (8) (0) (0) (0) (4))
      ((0) (2) (0) (4) (0) (3) (5) (9) (0))
      ((0) (0) (2) (0) (0) (0) (4) (0) (6))
      ((0) (3) (0) (9) (0) (6) (0) (1) (0))
      ((7) (0) (8) (0) (0) (0) (9) (0) (0))
      ((0) (7) (6) (8) (0) (4) (0) (2) (0))
      ((2) (0) (0) (0) (9) (0) (0) (0) (0))
      ((4) (0) (9) (3) (0) (1) (6) (0) (0)))
  "A valid Sudoku grid for testing")

(defparameter *g*
  (alexandria:copy-array *sdk/grid*)
  "A valid Sudoku grid for testing")

(defparameter *sdk/empty-cell-indices*
  nil
  "A list of the row and col indices of all the cells that are empty")

(defun my/utl/flatten
    (l)
  "
Takes an arbitrarily-nested list (l), and returns all its elements in a
single-level list."
  (cond ((null l) NIL)
	((atom l) (list l))
	(t (append (my/utl/flatten (car l)) (my/utl/flatten (cdr l))))))

(defun sdk/valid-pos-index-p
    (v #| Zero-based index |#)
  "Takes a number (v) denoting the zero-based index of a position of a number on
   a Sudoku grid, and returns true if v is valid
   Valid values for v are zero (0) through eight (8)"
  (member v *sdk/range-valid-indices*))

(defun sdk/get-val-at-pos
    (r #| The zero-based index of the position in its row |#
     c #| The zero-based index of the position in its column |#
     g #| A Sudoku grid |#)
  "Takes two numbers (r and c) denoting the row and column, respectively of a
   position (p) on a Sudoku grid and a Sudoku grid (g), and returns the
   value at p in g"
  (unless (and (sdk/valid-pos-index-p r) (sdk/valid-pos-index-p c))
    (error "
sdk/get-val-at-pos:
ERROR!
Invalid value provided for either Row or Column or both"))
    (aref g r c))

(defun my/utl/remove-nth
    (i  #| The index of the element to remove |#
     l  #| The list from which to remove an element |#)
  "Removes the element at the specified index
Takes a number (I) which denotes the index of the element to remove and a list
(L), and returns a new list sans the element at I"
  (nconc (subseq l 0 i) (nthcdr (1+ i) l)))

(defun sdk/get-all-row-values
    (r  #| The position of the number in its row |#
     g  #| The Sudoku grid |#)
  "   Takes a number (R) denoting the row-index of a position (P) and a Sudoku
   grid (G), and returns the values at all the positions that share the same
   row of G as P"
  (unless (sdk/valid-pos-index-p r)
    (error "
sdk/get-all-row-values:
ERROR!
Invalid value provided for Row"))
  (loop
	:for i
	:from 0 to 8
	:collect (sdk/get-val-at-pos r i g)))

(defun sdk/get-values
  (g)
  "
Takes a Sudoku grid (G), and returns all the values in G"
  (my/utl/flatten
   (loop
	:for r :from 0 :to 8
	:collect (mapcar #'car (sdk/get-all-row-values r g)))))

(defparameter *sdk/values*
  (sdk/get-values *sdk/grid*)
  "All the values in a Sudoku grid")

(defun sdk/get-co-row-values
    (r  #| The position of the number in its row |#
     c  #| The position of the number in its column |#
     g  #| The Sudoku grid |#)
  "Takes two numbers (R and C) denoting the row and column indices, respectively,
   of a position (P) on the Sudoku grid (G), and returns the values of all the
   positions that share the same row of G as P"
  (unless (sdk/valid-pos-index-p c)
    (error "
sdk/get-co-row-values:
ERROR!
Invalid value provided for Column"))
  (my/utl/remove-nth c (sdk/get-all-row-values r g)))

(defun sdk/get-all-col-values
    (c  #| The position of the number in its column |#
     g  #| The Sudoku grid |#)
  "   Takes a number (C) denoting the column-index of a position (P) and a
   Sudoku grid (G), and returns only the values of positions that share the same
   column of the Sudoku grid as P"
  (unless (sdk/valid-pos-index-p c)
    (error "
sdk/get-all-col-values:
ERROR!
Invalid value provided for Column"))
  (loop
    :for i
    :from 0 to 8
    :collect (sdk/get-val-at-pos i c g)))

(defun sdk/get-co-col-values
    (r  #| The position of the number in its row |#
     c  #| The position of the number in its column |#
     g  #| The Sudoku grid |#)
  "   Takes a number (C) denoting the column-index of a position (P) and a
   Sudoku grid (G), and returns only the values of positions that share the same
   column of the Sudoku grid as P"
  (unless (sdk/valid-pos-index-p r)
    (error "
sdk/get-co-col-values:
ERROR!
Invalid value provided for Row"))
  (my/utl/remove-nth r (sdk/get-all-col-values c g)))

(defun sdk/get-which-subgrid
    (r  #| The position of the number in its row |#
     c  #| The position of the number in its column |#)
"
Takes two numbers (R and C) denoting the row and column indices, respectively,
of a position (P) on the Sudoku grid (G), and returns the zero-based position of the subgrid, moving left to right and top to bottom"
  (unless (and (sdk/valid-pos-index-p r) (sdk/valid-pos-index-p c))
    (error "
ERROR!
Either the row-index or col-index is out of range, or both are"))
  (cond
    ;; First row of Sudoku grid
    ((and (member r '(0 1 2))
	  (member c '(0 1 2)))
     0)
    ((and (member r '(0 1 2))
	  (member c '(3 4 5)))
     1)
    ((and (member r '(0 1 2))
	  (member c '(6 7 8)))
     2)
    ;; Second row
    ((and (member r '(3 4 5))
	  (member c '(0 1 2)))
     3)
    ((and (member r '(3 4 5))
	  (member c '(3 4 5)))
     4)
    ((and (member r '(3 4 5))
	  (member c '(6 7 8)))
     5)
    ;; Third row
    ((and (member r '(6 7 8))
	  (member c '(0 1 2)))
     6)
    ((and (member r '(6 7 8))
	  (member c '(3 4 5)))
     7)
    ((and (member r '(6 7 8))
	  (member c '(6 7 8)))
     8)))

(defun sdk/get-subgrid-pos-indices
    (n)
  "
Takes a number (N) denoting the position of a subgrid of a Sudoku grid (G), and
returns a list of the possible row-indices and col-indices of the positions of values in G"
  (unless (member n *sdk/range-valid-indices*)
    (error "~%
ERROR!
The position of the subgrid is out of range"))
  (cond
    ;;First row of the Sudoku grid
    ((= n 0) (values '(0 1 2) '(0 1 2)))
    ((= n 1) (values '(0 1 2) '(3 4 5)))
    ((= n 2) (values '(0 1 2) '(6 7 8)))
    ;; Second row
    ((= n 3) (values '(3 4 5) '(0 1 2)))
    ((= n 4) (values '(3 4 5) '(3 4 5)))
    ((= n 5) (values '(3 4 5) '(6 7 8)))
    ;; Third row
    ((= n 6) (values '(6 7 8) '(0 1 2)))
    ((= n 7) (values '(6 7 8) '(3 4 5)))
    ((= n 8) (values '(6 7 8) '(6 7 8)))))

(defun sdk/get-all-subgrid-values
    (r  #| The position of the number in its row |#
     c  #| The position of the number in its column |#
     g  #| The Sudoku grid |#)
  "
Takes two numbers (R and C) denoting the row-index and column-index of a
position (P) and a Sudoku grid (G), and returns only the values at positions
that share the same 3x3 subgrid of the Sudoku grid as P"
  (unless (and (sdk/valid-pos-index-p r) (sdk/valid-pos-index-p c))
    (error "
ERROR!
Either the row-index or col-index is out of range, or both are"))
  (multiple-value-bind (rs cs)
      (sdk/get-subgrid-pos-indices (sdk/get-which-subgrid r c))
    ;(format t "~a, ~a~%~a~%" rs cs g)
    (let ((subgrid nil))
      (loop :named rows :for row :in rs :do
	(loop :named cols :for col :in cs :do
	  ;(format t "row is ~a, col is ~a~%" row col)
	  (push (sdk/get-val-at-pos row col g) subgrid)))
      (reverse subgrid))))

(defun sdk/get-subgrid-pos-index
    (r  #| The position of the number in its row |#
     c  #| The position of the number in its column |#)
  "
Takes two numbers (R and C) denoting the row and column indices, respectively
of a position (p) and a Sudoku grid (g), and returns the index of the position
in its 3x3 subgrid"
  (unless (and (sdk/valid-pos-index-p r) (sdk/valid-pos-index-p c))
    (error "
sdk/get-co-col-values:
ERROR!
Invalid value provided for Row or Col or both"))
  (cond
    ;; First rows of subgrids
    ((and (member r '(0 3 6)) (member c '(0 3 6))) 0)
    ((and (member r '(0 3 6)) (member c '(1 4 7))) 1)
    ((and (member r '(0 3 6)) (member c '(2 5 8))) 2)
    ;; Second rows of subgrids
    ((and (member r '(1 4 7)) (member c '(0 3 6))) 3)
    ((and (member r '(1 4 7)) (member c '(1 4 7))) 4)
    ((and (member r '(1 4 7)) (member c '(2 5 8))) 5)
    ;; Third rows of subgrids
    ((and (member r '(2 5 8)) (member c '(0 3 6))) 6)
    ((and (member r '(2 5 8)) (member c '(1 4 7))) 7)
    ((and (member r '(2 5 8)) (member c '(2 5 8))) 8)))

(defun sdk/get-co-subgrid-values
    (r  #| The position of the number in its row |#
     c  #| The position of the number in its column |#
     g  #| The Sudoku grid |#)
  "
Takes two numbers (R and C) denoting the row and col indices, respectively, of a position (P) and a Sudoku grid (G), and returns only the values of positions that share the same 3x3
subgrid of the Sudoku grid as P"
  (unless (and (sdk/valid-pos-index-p r) (sdk/valid-pos-index-p c))
    (error "
sdk/get-co-subgrid-values:
ERROR!
Invalid value provided for Row or Col or both"))
  (my/utl/remove-nth
   (sdk/get-subgrid-pos-index r c)
   (sdk/get-all-subgrid-values r c g)))

(defun sdk/get-co-values
    (r  #| The position of the number in its row |#
     c  #| The position of the number in its column |#
     g  #| The Sudoku grid |#)
  "
Takes two numbers (R and C) denoting the row and col indices, respectively, of a cell (C2) and a Sudoku grid (G), and returns the values in all the cells with which C2 shares the same row, col and 3x3 subgrid"
  (unless (and (sdk/valid-pos-index-p r) (sdk/valid-pos-index-p c))
    (error "
sdk/get-co-values:
ERROR!
Invalid value provided for Row or Col or both"))
  (append (sdk/get-co-row-values r c g)
	  (sdk/get-co-col-values r c g)
	  (sdk/get-co-subgrid-values r c g)))

(defun sdk/empty-cell-p
  (r  #| The position of the number in its row |#
     c  #| The position of the number in its column |#
   g  #| The Sudoku grid |#)
    "
Takes two numbers (R and C) denoting the row- and col indices of a cell (C2), and a Sudoku grid (G), and returns true is C2 is empty and false otherwise"
  (unless (and (sdk/valid-pos-index-p r) (sdk/valid-pos-index-p c))
    (error "
sdk/empty-cell-p:
ERROR!
Invalid value provided for Row or Col or both"))
  (zerop (sdk/get-val-at-pos r c g)))

(defun sdk/empty-cells-p
  (g)
  "
Takes a Sudoku grid (G), and returns true if there exist one or more empty
cells in G and false otherwise"
  (member 0 (sdk/get-values g)))

(defun sdk/get-empty-cell-indices
  (g)
  "
Takes a Sudoku grid (G), and returns the row and col indices of all the empty
cells in G"
  (let ((empty-cell-indices nil))
    (dotimes (r 9)
      (dotimes (c 9)
	(when (zerop (car (sdk/get-val-at-pos r c g)))
	  (push (list r c) empty-cell-indices))))
    (setf *sdk/empty-cell-indices* (reverse empty-cell-indices))))

(defun sdk/put-vals-in-cell
    (r  #| The position of the number in its row |#
     c  #| The position of the number in its column |#
     g  #| The Sudoku grid |#
     vs #| The value to place in the cell |#)
  "
Takes two numbers (R and C) denoting the row- and col indices of a position (P), a Sudoku grid (G) and numbers (VS) denoting the new values, and places VS in the cell at P"
  (unless (and (sdk/valid-pos-index-p r) (sdk/valid-pos-index-p c))
    (error "
sdk/put-vals-in-cell:
ERROR!
Invalid value provided for Row or Col or both"))
  (unless (every
	   #'(lambda (v)
	       (member v *sdk/range-valid-values-and-zero*))
	   vs)
    (error "
sdk/put-val-at-cell:
ERROR!
Invalid value(s) provided for cell"))
  (setf (aref g r c) vs))

(defun sdk/get-possible-values
    (r  #| The position of the number in its row |#
     c  #| The position of the number in its column |#
     g  #| The Sudoku grid |#)
  "
Takes two numbers (R and C) denoting the row and col indices, respectively, of a cell (C2) and a Sudoku grid (G), and returns all the possible valid values in C2"
  (let ((cvs (mapcar #'car (sdk/get-co-values r c g))))
    ;(format t "~a - " cvs)
    (loop
      :for i :from 1 :to 9
      :when (not (member i cvs))
	collect i into ps
      :finally (return ps))))

(defun sdk/put-possible-values
    (g  #| The Sudoku grid |#)
  "
Takes a Sudoku grid (G) containing empty cells (CS), and returns a new grid with all CS filled in with possible values"
  (let ((g2 (alexandria:copy-array g))
	(g3 (alexandria:copy-array g))
	(i 0)
	)
    (dolist (cell (sdk/get-empty-cell-indices g))
      (let* ((r  (first cell))
	     (c  (second cell))
	     (vs (sdk/get-possible-values r c g)))
	(format t "~a: ~a: ~a~%" (incf i) cell vs)
	(sdk/put-vals-in-cell r c g2 vs)
	(when (= (length vs) 1)
	  (sdk/put-vals-in-cell r c g3 vs))))
    (terpri)
    g3))

(defun sdk/print-grid
    (g           #| The grid to print |#
     &optional m #| An optional descriptive message |#)
  "
Takes a valid Sudoku grid (G) and an optional message (M), and pretty prints G"
  (format t "~%~a~%" m)
  (terpri)
  (loop :named rows :for row :from 0 :below 9 :do
    (loop :named cols :for col :from 0 :below 9 :do
      (format t "~a " (car (aref g row col))))
    (terpri))
  (terpri))

(defun sdk/solve
    (g)
  (labels ((sdk/help-solve (grid prvs-cnt-empty-cls)
	     (sdk/print-grid grid "Unsolved:")
	     (let ((crnt-cnt-empty-cls
		     (length (sdk/get-empty-cell-indices grid)))
		   (new-grid (sdk/put-possible-values grid)))
	       (sdk/print-grid new-grid "new-grid")
	       (format t
		       "~%crnt-cnt-empty-cls ~a~%prvs-cnt-empty-cls ~a~%"
		       crnt-cnt-empty-cls
		       prvs-cnt-empty-cls)
	       ;; If the number of empty cells has not reduced, then quit
	       (unless (< crnt-cnt-empty-cls prvs-cnt-empty-cls)
	       	 (sdk/print-grid new-grid "Unsolvable. Quitting.")
	       	 (return-from sdk/solve))
	       ;; When there are no more empty cells, then we are done
	       (when (= crnt-cnt-empty-cls 0)
		 (sdk/print-grid new-grid "Solved.")
		 (return-from sdk/solve))
	       (sdk/help-solve new-grid crnt-cnt-empty-cls))))
    (sdk/help-solve (alexandria:copy-array g) 81)))







































































;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; WARNING: CHECK ITS WORKING
(defmacro my/compose
      (o1  #| Function-object denoting the first fn to be applied |#
       o2  #| Function-object denoting the second fn to be applied |#
       &rest a   #| Single argument |# )
    "Implements function composition
  Takes two function-objects (o1 and o2) and an argument (a), then applies o1 to x and obtains a result (r), and finally returns the result obtained by applying o2 to r"
    `((lambda (x)
      (funcall ,o2 (funcall ,o1 x)))
      ,@a))

;;; WARNING: CHECK ITS WORKING
(defun paul-graham/compose (&rest fns)
  (destructuring-bind (fn1 . rest) (reverse fns)
    #'(lambda (&rest args)
        (reduce #'(lambda (v f) (funcall f v))
                rest
                :initial-value (apply fn1 args)))))

(defun my/utl-nat-p
    (n)
  "Takes a number (n), and returns true if n is a natural number, and false otherwise
   A number (n) is considered to be a natural number if n is -
     . an integer
     . either zero (0) or positive"
  (funcall #'(lambda (x) (and (integerp x)
			 (or (zerop x) (plusp x)))) n))

(defun my/utl-single-digit-nat-p
    (n)
  "Takes a number (n), and returns true if n is a single-digit natural number, and
   false otherwise
   A number (n) is considered to be a single-digit natural number if n is -
     . an integer
     . either zero (0) or positive
     . falls within the range of zero (0) and nine (9), both inclusive"
  (funcall #'(lambda (x) (and (my/utl-nat-p x)
			 (> x -1)
			 (< x 10))) n))

(defun sdk/valid-string-p
    (s)
  "Takes a string of integers (s), and returns true if s is a valid Sudoku row
   A row (r) is considered to be valid if it comprises exactly 9 numbers, where
   each number is
     . an integer
     . non-negative
     . in the range zero (0) to nine (9), both inclusive"
  (let ((l (my/utl/string-to-ints s)))
    (and (eql (length l) 9)
	 (every #'(lambda (x) (member x *sdk/range-valid-values-and-zero*)) l))))
