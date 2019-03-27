(in-package :utility)

(defun indexes-to-row-major-index (dimensions &rest subscripts)
  (apply #'+ (maplist (lambda (x y)
			(* (car x) (apply #'* (cdr y))))
		      subscripts dimensions)))

(defun row-major-index-to-indexes (index dimensions)
  (loop
     with idx = index
     with rank = (length dimensions)
     with indexes = (make-list rank)
     for dim-index from (- rank 1) downto 0
     do (setf (values idx (nth dim-index indexes))
              (floor idx (nth dim-index dimensions)))
     finally (return indexes)))

(defun indexes-to-column-major-index (dimensions &rest subscripts)
  (apply #'indexes-to-row-major-index
	 (cons (reverse dimensions) (reverse subscripts))))

(defun column-major-index-to-indexes (index dimensions)
  (reverse
   (funcall #'row-major-index-to-indexes
	    index (reverse dimensions))))
