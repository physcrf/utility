(in-package :utility)

(defalias 'emptyp 			'alexandria:emptyp)
(defalias 'rotate 			'alexandria:rotate)
(defalias 'random-elt 			'alexandria:random-elt)
(defalias 'copy-sequence 		'alexandria:copy-sequence)
(defalias 'first-elt 			'alexandria:first-elt)
(defalias 'last-elt 			'alexandria:last-elt)
(defalias 'split-sequence 		'split-sequence:split-sequence)
(defalias 'split-sequence-if 		'split-sequence:split-sequence-if)
(defalias 'split-sequence-if-not 	'split-sequence:split-sequence-if-not)
(defalias 'runs 			'serapeum:runs)
(defalias 'batches 			'serapeum:batches)
(defalias 'frequencies 			'serapeum:frequencies)
(defalias 'assort 			'serapeum:assort)
(defalias 'partition 			'serapeum:partition)
(defalias 'do-each 			'serapeum:do-each)
(defalias 'filter 			'serapeum:filter)
(defalias 'keep 			'serapeum:keep)
(defalias 'single 			'serapeum:single)
(defalias 'cumulate 			'serapeum:scan)
(defalias 'length= 			'alexandria:length=)
(defalias 'length> 			'serapeum:length>)
(defalias 'length< 			'serapeum:length<)
(defalias 'length>= 			'serapeum:length>=)
(defalias 'length<= 			'serapeum:length<=)
(defalias 'longer 			'serapeum:longer)
(defalias 'take 			'serapeum:take)
(defalias 'drop 			'serapeum:drop)

(defun second-elt (sequence)
  "Returns the second element of SEQUENCE. Signals a type-error if
SEQUENCE is not a sequence, or it is an empty sequence."
  (check-type sequence (and sequence (not (satisfies emptyp))))
  (elt sequence 1))

(defun (setf second-elt) (object sequence)
  (check-type sequence (and sequence (not (satisfies emptyp))))
  (setf (elt sequence 1) object))

(defun third-elt (sequence)
  "Returns the third element of SEQUENCE. Signals a type-error if
SEQUENCE is not a sequence, or it is an empty sequence."
  (check-type sequence (and sequence (not (satisfies emptyp))))
  (elt sequence 2))

(defun (setf third-elt) (object sequence)
  (check-type sequence (and sequence (not (satisfies emptyp))))
  (setf (elt sequence 2) object))

(defun of-length (n sequence)
  "of-length (n sequence)
  Checks if the length of sequence is n."
  (= n (length sequence)))

(defun longest (&rest sequences)
  "longest (&rest sequences)
  Returns the longest sequence in sequences."
  (funcall #'serapeum:longest sequences))

(export '(emptyp
	  rotate
	  random-elt
	  copy-sequence
	  first-elt
	  second-elt
	  third-elt
	  last-elt
	  split-sequence
	  split-sequence-if
	  split-sequence-if-not
	  runs
	  batches
	  frequencies
	  assort
	  partition
	  do-each
	  filter
	  keep
	  single
	  cumulate
	  of-length
	  length=
	  length>
	  length<
	  length>=
	  length<=
	  longer
	  longest
	  take
	  drop))
