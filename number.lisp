(in-package :utility)

(defalias 'parse-float 			'parse-float:parse-float)
(defalias 'parse-number 		'parse-number:parse-number)
(defalias 'parse-real-number 		'parse-number:parse-real-number)
(defalias 'parse-positive-real-number 	'parse-number:parse-positive-real-number)
(defalias 'bits 			'serapeum:bits)
(defalias 'unbits 			'serapeum:unbits)
(defalias 'random-in-range 		'serapeum:random-in-range)
(defalias 'iota 			'alexandria:iota)

(defun string-integerp (string)
  "string-integerp (string)

  Tells if a string STRING represents an integer."
  (serapeum:true (ppcre:scan "^\\s*[-+]?[0-9]+\\.?\\s*$" (str:trim string))))

(defun string-floatp (string)
  "string-floatp (string)

  Tells if a string STRING represents a float number."
  (serapeum:true (ppcre:scan "^\\s*[-+]?[0-9]*\\.[0-9]+([eEdD][-+]?[0-9]+)?\\s*$"
			     (str:trim string))))

(defun string-realp (string)
  "string-realp (string)

  Tells if a string STRING represents a real number."
  (serapeum:true (ppcre:scan "^\\s*[-+]?[0-9]*\\.?[0-9]+([eEdD][-+]?[0-9]+)?\\s*$"
			     (str:trim string))))

(defun string-complexp (string)
  "string-complexp (string)

  Tells if a string STRING represents a complex number."
  (let ((real-regex "[-+]?[0-9]*\\.?[0-9]+([eEdD][-+]?[0-9]+)?")
	(prefix-regex "^\\s*#[cC]\\(\\s*")
	(suffix-regex "\\s*\\)\\s*$"))
    (serapeum:true
     (and
      (not (ppcre:scan (str:concat prefix-regex
				   real-regex "\\s+"
				   "0+\\.?"
				   suffix-regex)
		       string))
      (ppcre:scan (str:concat prefix-regex
			      real-regex "\\s+"
			      real-regex
			      suffix-regex)
		  string)))))

(defun string-numberp (string)
  "string-numberp (string)

  Tells if a string STRING represents a number."
  (or (string-realp string)
      (string-complexp string)))

(export '(parse-float
	  parse-number
	  parse-real-number
	  parse-positive-real-number
	  bits
	  unbits
	  random-in-range
	  iota
	  string-integerp
	  string-floatp
	  string-realp
	  string-complexp
	  string-numberp))
