(in-package :utility)

(defun indexes-to-row-major-index (dimensions &rest subscripts)
  "indexes-to-row-major-index (dimensions &rest subscripts) 

  Transforms subscripts into a row major index with respect to
dimensions."
  (apply #'+ (maplist (lambda (x y)
			(* (car x) (apply #'* (cdr y))))
		      subscripts dimensions)))

(defun row-major-index-to-indexes (index dimensions)
  "row-major-index-to-indexes (index dimensions)

  Transforms a row major index into subscripts with respect to 
dimensions."
  (loop
     with idx = index
     with rank = (length dimensions)
     with indexes = (make-list rank)
     for dim-index from (- rank 1) downto 0
     do (setf (values idx (nth dim-index indexes))
              (floor idx (nth dim-index dimensions)))
     finally (return indexes)))

(defun indexes-to-column-major-index (dimensions &rest subscripts)
  "indexes-to-column-major-index (dimensions &rest subscripts)

  Transforms subscripts into a column major index with respect to 
dimensions."
  (apply #'indexes-to-row-major-index
	 (cons (reverse dimensions) (reverse subscripts))))

(defun column-major-index-to-indexes (index dimensions)
  "column-major-index-to-indexes (index dimensions)

  Transforms a column major index into subscripts with respect to 
dimensions."
  (reverse
   (funcall #'row-major-index-to-indexes
	    index (reverse dimensions))))

(export '(indexes-to-row-major-index
	  row-major-index-to-indexes
	  indexes-to-column-major-index
	  column-major-index-to-indexes))
