(in-package :utility)

(import 'alexandria:appendf)
(import 'alexandria:lastcar)
(import 'serapeum:append1)
(import 'serapeum:in)
(import 'serapeum:plist-keys)
(import 'serapeum:plist-values)

(defmacro insert (position object list)
  `(progn
     (if (zerop ,position)
	 (push ,object ,list)
	 (push ,object (cdr (nthcdr (1- ,position) ,list))))
     ,list))

