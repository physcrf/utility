(in-package :utility)

(import 'alexandria:appendf)
(import 'serapeum:append1)
(import 'alexandria:lastcar)
(import 'serapeum:plist-keys)
(import 'serapeum:plist-values)

(defmacro insert (position object list)
  "insert (position object list)

  Inserts object into list at position." 
  `(progn
     (if (zerop ,position)
	 (push ,object ,list)
	 (push ,object (cdr (nthcdr (1- ,position) ,list))))
     ,list))

