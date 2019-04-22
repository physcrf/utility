(in-package :utility-test)

(define-test indexes-to-row-major-index-test
  (assert-equal 3 (indexes-to-row-major-index '(3 2) 1 1))
  (assert-equal 4 (indexes-to-row-major-index '(2 3) 1 1)))

(define-test row-major-index-to-indexes-test
  (assert-equal '(1 1) (row-major-index-to-indexes 3 '(3 2)))
  (assert-equal '(1 1) (row-major-index-to-indexes 4 '(2 3))))

(define-test indexes-to-column-major-index-test
  (assert-equal 4 (indexes-to-column-major-index '(3 2) 1 1))
  (assert-equal 3 (indexes-to-column-major-index '(2 3) 1 1)))

(define-test column-major-index-to-indexes-test
  (assert-equal '(1 1) (column-major-index-to-indexes 4 '(3 2)))
  (assert-equal '(1 1) (column-major-index-to-indexes 3 '(2 3))))

(define-test copy-array-test
  (assert-equalp #2A((1 2) (3 4)) (copy-array #2A((1 2) (3 4))))
  (assert-equalp #(1 2 3 4) (copy-array #(1 2 3 4))))
