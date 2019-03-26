(in-package :utility)

(import 'alexandria:make-keyword)
(import 'alexandria:symbolicate)
(import 'serapeum:find-keyword)

;; string->keyword
;; (setf (symbol-function 'string->keyword) (symbol-function 'make-keyword))
;; keyword->string
;; (setf (symbol-function 'keyword->string) (symbol-function 'symbol-name))
;; string->symbol
;; (setf (symbol-function 'string->symbol) (symbol-function 'intern))
;; symbol->string
;; (setf (symbol-function 'symbol-string) (symbol-function 'symbol-name))
