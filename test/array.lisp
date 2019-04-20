(in-package :utility-test)

(define-test indexes-to-row-major-index-test
  (assert-equal 3 (indexes-to-row-major-index '(3 2) 1 1))
  (assert-equal 4 (indexes-to-row-major-index '(2 3) 1 1)))

(define-test row-major-index-to-indexes-test
  (assert-equal '(1 1) (row-major-index-to-indexes 3 '(3 2)))
  (assert-equal '(1 1) (row-major-index-to-indexes 4 '(2 3))))

(defun array-test ()
  (run-tests '(indexes-to-row-major-index-test
	       row-major-index-to-indexes-test)))
