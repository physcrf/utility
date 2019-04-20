(in-package :utility)

(defalias 'switch	'alexandria:switch)
(defalias 'cswitch	'alexandria:cswitch)
(defalias 'eswitch	'alexandria:eswitch)
(defalias 'select 	'serapeum:select)
(defalias 'select* 	'serapeum:selector)
(defalias 'eq* 		'serapeum:eq*)
(defalias 'eql* 	'serapeum:eql*)
(defalias 'equal* 	'serapeum:equal*)
(defalias 'equalp* 	'serapeum:equalp*)

(export '(switch
	  cswitch
	  eswitch
	  select
	  select*
	  eq*
	  eql*
	  equal*
	  equalp*))
