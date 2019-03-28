(in-package :utility)

(import 'alexandria:emptyp)
(import 'alexandria:rotate)
(import 'alexandria:random-elt)
(import 'alexandria:copy-sequence)
(import 'alexandria:first-elt)
(import 'alexandria:last-elt)
(import 'split-sequence:split-sequence)
(import 'split-sequence:split-sequence-if)
(import 'split-sequence:split-sequence-if-not)
(import 'serapeum:runs)
(import 'serapeum:batches)
(import 'serapeum:frequencies)
(import 'serapeum:assort)
(import 'serapeum:partition)
(import 'serapeum:do-each)
(import 'serapeum:filter)
(import 'serapeum:keep)
(import 'serapeum:single)
(import 'serapeum:scan)
(import 'alexandria:length=)
(import 'serapeum:length>)
(import 'serapeum:length<)
(import 'serapeum:length>=)
(import 'serapeum:length<=)
(import 'serapeum:longer)
;;(import 'serapeum:longest)
(import 'serapeum:take)
(import 'serapeum:drop)

(defalias cumulate #'scan)

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
