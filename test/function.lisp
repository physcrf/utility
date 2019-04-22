(in-package :utility-test)

(define-test disjoin-test
  (assert-eq t (funcall (disjoin #'zerop #'oddp) 0))
  (assert-eq t (funcall (disjoin #'zerop #'oddp) 1))
  (assert-eq nil (funcall (disjoin #'zerop #'oddp) 2)))

(define-test conjoin-test
  (assert-eq t (funcall (conjoin #'zerop #'evenp) 0))
  (assert-eq nil (funcall (conjoin #'zerop #'evenp) 1))
  (assert-eq nil (funcall (conjoin #'zerop #'evenp) 2)))

(define-test compose-test
  (assert-eq 'b (funcall (compose #'car #'cdr) '(a b c)))
  (assert-eq 'c (funcall (compose #'car #'cdr #'cdr) '(a b c))))

(define-test curry-test
  (assert-number-equal 2 (funcall (curry #'+ 1) 1))
  (assert-number-equal 3 (funcall (curry #'+ 2) 1)))
  
