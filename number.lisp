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

  Tells if a string represents an integer."
  (serapeum:true (ppcre:scan "^\\s*[-+]?[0-9]+\\.?\\s*$" (str:trim string))))

(defun string-floatp (string)
  "string-floatp (string)

  Tells if a string represents a float number."
  (serapeum:true (ppcre:scan "^\\s*[-+]?[0-9]*\\.[0-9]+([eEdD][-+]?[0-9]+)?\\s*$"
			     (str:trim string))))

(defun string-realp (string)
  "string-realp (string)

  Tells if a string represents a real number."
  (serapeum:true (ppcre:scan "^\\s*[-+]?[0-9]*\\.?[0-9]+([eEdD][-+]?[0-9]+)?\\s*$"
			     (str:trim string))))

(defun string-complexp (string)
  "string-complexp (string)

  Tells if a string represents a complex number."
  (let ((real-number-regex "[-+]?[0-9]*\\.?[0-9]+([eEdD][-+]?[0-9]+)?")
	(prefix-regex "^\\s*#[cC]\\(\\s*")
	(suffix-regex "\\s*\\)\\s*$"))
    (serapeum:true
     (if (ppcre:scan "\\s*#[cC]\\(\\s*[^\\s]+\\s+0\\s*\\)\\s*$" string)
	 nil
	 (ppcre:scan (str:concat prefix-regex
				 real-number-regex "\\s+"
				 real-number-regex
				 suffix-regex)
		     string)))))
    
(defun string-numberp (string)
  "string-numberp (string)

  Tells if a string represents a number."
  (let ((real-number-regex "[-+]?[0-9]*\\.?[0-9]+([eEdD][-+]?[0-9]+)?")
	(prefix-regex "^\\s*#[cC]\\(\\s*")
	(suffix-regex "\\s*\\)\\s*$"))
    (if (eql (alexandria:first-elt string) #\#)
	(serapeum:true (ppcre:scan
			(str:concat prefix-regex
				    real-number-regex "\\s+"
				    real-number-regex
				    suffix-regex)
			string))
	(string-realp string))))
				        
(export '(parse-float
	  parse-number
	  parse-real-number
	  parse-positive-real-number
	  bits
	  unbits
	  random-in-range
	  iota))
