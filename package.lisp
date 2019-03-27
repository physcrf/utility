(defpackage :utility
  (:use :cl)
  (:import-from :alexandria
		:length=)
  (:import-from :serapeum
		:defalias)
  (:export ;; array
   :indexes-to-row-major-index :row-major-index-to-indexes
   :indexes-to-column-major-index :column-major-index-to-indexes)
  (:export ;; control-flow
   :select :select* 
   :eq* :eql* :equal* :equalp*)
  (:export ;;function
   :disjoin :conjoin :compose
   :curry :rcurry
   :nested-loop :nested-map)
  (:export ;; hash-table
   ;; :list-hash-set
   :do-hash-table
   :dict :dict*
   :copy-hash-table
   :hash-table-keys :hash-table-values
   :hash-table-alist :hash-table-plist
   :alist-hash-table :plist-hash-table)
  (:export ;; list
   :appendf :append1
   :lastcar :in
   :plist-keys :plist-values
   :insert)
  (:export ;; macro
   :with-gensyms)
  (:export ;; number
   :parse-number :parse-real-number
   :parse-positive-real-number
   :bits :unbits
   :random-in-range)
  (:export ;; sequence
   :emptyp :rotate 
   :random-elt :first-elt :last-elt
   :split-sequence :split-sequence-if :split-sequence-if-not
   :runs :batches :assort :partition
   :do-each :filter
   :keep :single
   :frequencies :cumulate
   :of-length
   :length= :length> :length< :length>= :length<=
   :longer :longest
   :take :drop)
  (:export ;; symbols
   :make-keyword :symbolicate :find-keyword)
  (:export ;; types
   :true))
   
   
   
   
