(in-package :utility)

(defalias 'parse-float 			'parse-float:parse-float)
(defalias 'parse-number 		'parse-number:parse-number)
(defalias 'parse-real-number 		'parse-number:parse-real-number)
(defalias 'parse-positive-real-number 	'parse-number:parse-positive-real-number)
(defalias 'bits 			'serapeum:bits)
(defalias 'unbits 			'serapeum:unbits)
(defalias 'random-in-range 		'serapeum:random-in-range)
(defalias 'iota 			'alexandria:iota)

(export '(parse-float
	  parse-number
	  parse-real-number
	  parse-positive-real-number
	  bits
	  unbits
	  random-in-range
	  iota))
