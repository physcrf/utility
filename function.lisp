(in-package :utility)

(import 'alexandria:disjoin)
(import 'alexandria:conjoin)
(import 'alexandria:compose)
(import 'alexandria:curry)
(import 'alexandria:rcurry)

;; https://stackoverflow.com/questions/10163298/lisp-macro-or-function-for-nested-loops
;; Wonderful Code
(defmacro nested-loop (subscripts dimensions &body body)
  "nested-loop (subscripts dimensions &body body)

  This macro does nested loop over dimensions."
  (when (not (length= dimensions subscripts))
    (error "nested-loop: dimensions and subscripts are not of same length."))
  (loop
     for index in (reverse subscripts)
     for dim in (reverse dimensions)
     for x = body then (list y)
     for y = `(dotimes (,index ,dim) ,@x)
     finally (return y)))

(defun nested-map (dimensions function)
  "nested-map (dimensions function)

   This function nested loops over dimensions with function."
  (labels ((fn (args dimensions)
	     (if dimensions
		 (dotimes (i (car dimensions))
		   (fn (cons i args) (cdr dimensions)))
	       (apply function (reverse args)))))
    (fn nil dimensions)))

(export '(disjoin
	  conjoin
	  compose
	  curry
	  rcurry
	  nested-loop
	  nested-map))
