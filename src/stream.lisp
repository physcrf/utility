(in-package :utility)

(defalias 'read-file-form 	'uiop:read-file-form)
(defalias 'read-file-forms 	'uiop:read-file-forms)
(defalias 'read-file-line 	'uiop:read-file-line)
(defalias 'read-file-lines 	'uiop:read-file-lines)
(defalias 'read-file-string 	'uiop:read-file-string)

(defun read-file-data (filename &key (comments "#") (delimiter "\\s+"))
  "read-file-data (filename &key (comments \"#\") (delimiter \"\\s+\"))

  Reads data from file filename into a nested list"
  (let* ((lines (read-file-lines filename))
	 (lines (mapcan (lambda (line)
			  (let ((clean-line
				 (str:trim (ppcre:regex-replace
					    (str:concat comments ".*$") line ""))))
			    (when (not (str:blankp clean-line))
			      (list clean-line))))
			lines))
	 (lines (mapcar (lambda (line) (ppcre:split delimiter line)) lines))
	 (lines (loop for line in lines
		      collect (mapcar #'parse-number:parse-number line))))
    lines))

(defun write-file-data (filename data &key
				 (format "~10,8G") (delimiter "~8T") (comments "#")
				 (header "") (footer ""))
  "write-file-data (filename data &key
			    (format \"~10,8G\") (delimiter \"~8T\") (comments \"#\")
			    (header \"\") (footer \"\"))"
  (with-open-file (stream filename
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
    (when (not (str:blankp header))
      (format stream "~A ~A~%" comments header))
    (loop for line in data
	  do (format stream (str:concat "~{" format "~^" delimiter "~}~%") line))
    (when (not (str:blankp footer))
      (format stream "~A~A~%" comments footer))))

(defmacro with-input-file ((stream filespec) &body body)
  "with-input-file ((stream filespec) &body body)

  A shorthand of WITH-OPEN-FILE with (:DIRECTION :INPUT)"
  `(with-open-file (,stream ,filespec
			    :direction :input)
     ,@body))

(defmacro with-output-file ((stream filespec) &body body)
  "with-output-file ((stream filespec) &body body)

  A shorthand of WITH-OPEN-FILE with (:DIRECTION :OUTPUT
                                      :IF-EXISTS :SUPERSEDE
                                      :IF-DOES-NOT-EXIST :CREATE)"
  `(with-open-file (,stream ,filespec
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
     ,@body))
  
(export '(read-file-form
	  read-file-forms
	  read-file-line
	  read-file-lines
	  read-file-string
	  read-file-data
	  write-file-data
	  with-input-file
	  with-output-file))
