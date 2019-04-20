(in-package :utility)

(defalias 'appendf 		'alexandria:appendf)
(defalias 'append1 		'serapeum:append1)
(defalias 'lastcar 		'alexandria:lastcar)
(defalias 'plist-keys 		'serapeum:plist-keys)
(defalias 'plist-values 	'serapeum:plist-values)

(defmacro insert (position object list)
  "insert (position object list)

  Inserts object into list at position." 
  `(progn
     (if (zerop ,position)
	 (push ,object ,list)
	 (push ,object (cdr (nthcdr (1- ,position) ,list))))
     ,list))

(export '(appendf
	  append1
	  lastcar
	  plist-keys
	  plist-values
	  insert))
