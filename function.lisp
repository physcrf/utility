(in-package :utility)

(defun defalias (alias macro-or-function &optional docstring)
  "defalias (alias function &optional docstring)

  Defines alias of macro-or-fucntion with optional docstring. If
docstring is not NIL, the old documentation would be replaced by
docstring."
  (if (macro-function macro-or-function)
      (setf (macro-function alias) (macro-function macro-or-function))
      (setf (fdefinition alias) (fdefinition macro-or-function)))
  (when docstring
    (setf (documentation alias 'function) docstring))
  alias)

(defalias 'disjoin	'alexandria:disjoin)
(defalias 'conjoin	'alexandria:conjoin)
(defalias 'compose 	'alexandria:compose)
(defalias 'curry 	'alexandria:curry)
(defalias 'rcurry 	'alexandria:rcurry)

;; https://stackoverflow.com/questions/10163298/lisp-macro-or-function-for-nested-loops
;; Wonderful Code
(defmacro nested-loop (subscripts dimensions &body body)
  "nested-loop (subscripts dimensions &body body)

  This macro does nested loop over dimensions."
  (when (not (alexandria:length= dimensions subscripts))
    (error "nested-loop: dimensions and subscripts are not of same length."))
  (loop for index in (reverse subscripts)
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

(export '(defalias
	  disjoin
	  conjoin
	  compose
	  curry
	  rcurry
	  nested-loop
	  nested-map))
