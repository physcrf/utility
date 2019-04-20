(in-package :utility)

(defalias 'make-keyword 	'alexandria:make-keyword)
(defalias 'symbolicate 		'alexandria:symbolicate)
(defalias 'find-keyword 	'serapeum:find-keyword)

(export '(make-keyword
	  symbolicate
	  find-keyword))
