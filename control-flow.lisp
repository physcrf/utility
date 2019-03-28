(in-package :utility)

(defalias 'select 'serapeum:select)
(defalias 'select* 'serapeum:selector)
(defalias 'eq* 'serapeum:eq*)
(defalias 'eql* 'serapeum:eql*)
(defalias 'equal* 'serapeum:equal*)
(defalias 'equalp* 'serapeum:equalp*)

(export '(select
	  select*
	  eq*
	  eql*
	  equal*
	  equalp*))
