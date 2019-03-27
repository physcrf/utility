(in-package :utility)

(import 'uiop:list-to-hash-set)
(import 'alexandria:copy-hash-table)
(import 'alexandria:hash-table-keys)
(import 'alexandria:hash-table-values)
(import 'alexandria:hash-table-alist)
(import 'alexandria:hash-table-plist)
(import 'alexandria:alist-hash-table)
(import 'alexandria:plist-hash-table)
(import 'serapeum:do-hash-table)

(defalias list-hash-set #'list-to-hash-set)
