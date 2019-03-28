(in-package :utility)

(defalias 'dict        		'serapeum:dict)
(defalias 'dict*       		'serapeum:dict*)
(defalias 'do-hash-table       	'serapeum:do-hash-table)
(defalias 'copy-hash-table 	'alexandria:copy-hash-table)
(defalias 'hash-table-keys 	'alexandria:hash-table-keys)
(defalias 'hash-table-values 	'alexandria:hash-table-values)
(defalias 'hash-table-alist 	'alexandria:hash-table-alist)
(defalias 'hash-table-plist 	'alexandria:hash-table-plist)
(defalias 'alist-hash-table 	'alexandria:alist-hash-table)
(defalias 'plist-hash-table 	'alexandria:plist-hash-table)

(export '(dict
	  dict*
	  do-hash-table
	  copy-hash-table
	  hash-table-keys
	  hash-table-values
	  hash-table-alist
	  hash-table-plist
	  alist-hash-table
	  plist-hash-table))
