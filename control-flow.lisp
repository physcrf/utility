(in-package :utility)

(import 'serapeum:select)
(import 'serapeum:selector)
(import 'serapeum:eq*)
(import 'serapeum:eql*)
(import 'serapeum:equal*)
(import 'serapeum:equalp*)

(setf (macro-function 'select*) (macro-function 'selector))

(export '(select
	  select*
	  eq*
	  eql*
	  equal*
	  equalp*))
