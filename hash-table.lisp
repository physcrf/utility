(in-package :utility)

(import 'serapeum:dict)
(import 'serapeum:dict*)
(import 'serapeum:do-hash-table)
(import 'alexandria:copy-hash-table)
(import 'alexandria:hash-table-keys)
(import 'alexandria:hash-table-values)
(import 'alexandria:hash-table-alist)
(import 'alexandria:hash-table-plist)
(import 'alexandria:alist-hash-table)
(import 'alexandria:plist-hash-table)

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
