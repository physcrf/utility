# utility
My utility collection

## Introduction
There are serveral external generic utility libraries, such as famous
[Alexandria](https://common-lisp.net/project/alexandria/) in Common
Lisp world. However, usually `Alexandria` alone is not enough for
progeraming, and one need to import other libraries such as de facto
standard utility
[uiop](https://common-lisp.net/project/asdf/uiop.html) or
[serapeum](https://github.com/ruricolist/serapeum), a large supplement
to `Alexandria`. At the same time, sometimes there are name conflicts
between different packages and one need to choose the needed function
by own hand. 

Therefore I just collect functions from multiple libraries into one
single package for my own use. Some functions are also renamed to fit
my convention. Right now, used libraries are
- [Alexandria](https://common-lisp.net/project/alexandria/)
- [serapeum](https://github.com/ruricolist/serapeum)
- [uiop](https://common-lisp.net/project/asdf/uiop.html)
- [parse-number](https://github.com/sharplispers/parse-number)
- [split-sequence](https://www.cliki.net/SPLIT-SEQUENCE)

This collection focus on miscellaneous utilities, thus some
professional libraries such as
[cl-ppcre](https://edicl.github.io/cl-ppcre/) are not included. 

## Funtion List
### [array](#array-ref)
- [indexes-to-row-major-index](#indexes-to-row-major-index)
- [row-major-index-to-indexes](#row-major-index-to-indexes)
- [indexes-to-column-major-index](#indexes-to-column-major-index)
- [column-major-index-to-indexes](#column-major-index-to-indexes)
### [control flow](#control-flow-ref)
- [select](#select)
- [select*](#select*)
- [eq*](#eq*)
- [eql*](#eql*)
- [equal*](#equal*)
- [equalp*](#equalp*)
### [function](#function-ref)
- [disjoin](#disjoin)
- [conjoin](#conjoin)
- [compose](#compose)
- [curry](#curry)
- [rcurry](#rcurry)
- [nested-loop](#nested-loop)
- [nested-map](#nested-map)
### [hash table](#hash-table-ref)
- [list-hash-set](#list-hash-set)
- [copy-hash-table](#copy-hash-table)
- [hash-table-keys](#hash-table-keys)
- [hash-table-values](#hash-table-values)
- [hash-table-alist](#hash-table-alist)
- [hash-table-plist](#hash-table-plist)
- [alist-hash-table](#alist-hash-table)
- [plist-hash-table](#plist-hash-table)
- [do-hash-table](#do-hash-table)
### [list](#list-ref)
- [appendf](#appendf)
- [lastcar](#lastcar)
- [append1](#append1)
- [in](#in)
- [plist-keys](#plist-keys)
- [plist-values](#plist-values)
- [insert](#insert)
### [macro](#macro-ref)
- [with-gensyms](#with-gensyms)
### [number](#number-ref)
- [parse-number](#parse-number)
- [parse-real-number](#parse-real-number)
- [parse-positive-real-number](#parse-positive-real-number)
- [bits](#bits)
- [unbits](#unbits)
### [sequence](#sequence-ref)
- [emptyp](#emptyp)
- [rotate](#rotate)
- [random-elt](#random-elt)
- [first-elt](#first-elt)
- [last-elt](#last-elt)
- [split-sequence](#split-sequence)
- [split-sequence-if](#split-sequence-if)
- [split-sequence-if-not](#split-sequence-if-not)
- [runs](#run)
- [batches](#batch)
- [assort](#assort)
- [partition](#partition)
- [do-each](#do-each)
- [filter](#filter)
- [keep](#keep)
- [single](#single)
- [frequencies](#frequencies)
- [scan](#scan)
- [length=](#length=)
- [length>](#length>)
- [length<](#length<)
- [length>=](#length>=)
- [length<=](#length<=)
- [longer](#longer)
- [longest](#longest)
- [take](#take)
- [drop](#drop)
### [stream](#stream-ref)
- [read-file-form](#read-file-form)
- [read-file-forms](#read-file-forms)
- [read-file-line](#read-file-line)
- [read-file-lines](#read-file-lines)
- [read-file-string](#read-file-string)
### [symbols](#symbols-ref)
- [make-keyworkd](#make-keyworkd)
- [symbolicate](#symbolicate)
- [find-keyword](#find-keyword)
### [types](#types-ref)
- [true](#true)
## Function Reference
### <span id="array-ref"> array </span>
#### <span id="indexes-to-row-major-index"> indexes-to-row-major-index (dimensions &rest subscripts) </span>
This function is written in reference to
[cffi](https://common-lisp.net/project/cffi/)'s internal utilities
(which are not exported by `cffi`). It transforms `subscripts` into a
row major index with respect to `dimensions`.

Examples:
```cl
(indexes-to-row-major-index '(4 5) 2 1) ;; => 11
(indexes-to-row-major-index '(2 2) 1 0) ;; => 2
```

#### <span id="row-major-index-to-indexes"> row-major-index-to-indexes (index dimensions) </span>
Transforms a row major `index` into subscripts with respect to
`dimensions`.

Examples:
```cl
(row-major-index-to-indexes 2 '(2 2)) ;; => (1 0)
(row-major-index-to-indexes 11 '(4 5)) ;; => (2 1)
```

#### <span id="indexes-to-column-major-index"> indexes-to-column-major-index (dimensions &rest subscripts) </span>
Transforms `subscripts` into a column major index with respect to
`dimensions`.

Examples:
```cl
(indexes-to-column-major-index '(4 5) 2 1) ;; => 6
(indexes-to-column-major-index '(2 2) 1 0) ;; => 1
```

#### <span id="column-major-index-to-indexes"> column-major-index-to-indexes (index dimensions) </span>
Transforms a column major `index` into subscripts with respect to `dimensions`.

Examples:
```cl
(column-major-index-to-indexes 6 '(4 5)) ;; => (2 1)
(column-major-index-to-indexes 1 '(2 2)) ;; => (1 0)
```
### <span id="control-flow-ref"> control flow </span>
#### <span id="select"> select (keyform &body clauses) </span>
Alias of `serapeum:select`, see
[serapeum](https://github.com/ruricolist/serapeum/blob/master/REFERENCE.md).
Like `cl:case`, but keys are evaluated. Note that, interprets a list
as the first element of a clause as a list of keys. To use a form as a
key, you must add an extra set of parentheses.

Examples:
```cl
(select 4
	   (((+ 1 1)) 'two)
	   (((+ 1 2)) 'three)
	   (((+ 2 2)) 'four)
	   (t t))
;; => FOUR
```

#### <span id="select*"> select* (keyform test &body clauses) </span>
Alias of `serapeum:selector`. Like `serapeum:select`, but compare
using `test`. Note that `test` is not evaluated.

Examples:
```cl
(select* 4.0 eq
	   (((+ 1 1)) 'two)
	   (((+ 1 2)) 'three)
	   (((+ 2 2)) 'four)
	   (t t))
;; => T

(select* 4.0 =
	   (((+ 1 1)) 'two)
	   (((+ 1 2)) 'three)
	   (((+ 2 2)) 'four)
	   (t t))
;; => FOUR
```

#### <span id="eq*"> eq* (&rest objects) </span>
Alias of `serapeum:eq*`, variadic version of `cl:eq`.
- with no arguments, return `T`.
- with one arguments, return `T`.
- with two arguments, same as `cl:eq`.
- with more arguments, return `T` only if all `objects` are equivalent
  under `cl:eq`.
This function is useful when trying to compare more than two arguments.

Examples:
```cl
(eq*) ;; => T
(eq* 'a) ;; => T
(eq* 'a 'a) ;; => T
(eq* 'a 'b) ;; => NIL
(eq* 'a 'a 'a) ;; => T
(eq* 'a 'a 'b) ;; => NIL
```

#### <span id="eql*"> eql* (&rest objects) </span>
Alias of `serapeum:eql*`, variadic version of `cl:eql`. Usage is the
same as [`eq*`](#eq*) except using `cl:eql` to compare.

#### <span id="equal*"> equal* (&rest objects) </span>
Alias of `serapeum:equal*`, variadic version of `cl:equal`. Usage is the
same as [`eq*`](#eq*) except using `cl:equal` to compare.

#### <span id="equalp*"> equalp* (&rest objects) </span>
Alias of `serapeum:equalp*`, variadic version of `cl:equalp`. Usage is
the same as [`eq*`](#eq*) except using `cl:equalp` to compare.


### <span id="function-ref"> function </span>
#### <span id="disjoin"> disjoin (predicate &rest more-predicates) </span>
Alias of `alexandria:disjoin`, see
[Alexandria](https://common-lisp.net/project/alexandria/draft/alexandria.html). Returns
a function that applies each of `predicate` and `more-predicate`
functions in turn to its arguments, returning the primary value of the
first predicate that returns true, without calling the remaining
predicates. If none of the predicates returns true, `NIL` is returned.

Examples:
```cl
(funcall (disjoin #'zerop #'oddp) 0) ;; => T
(funcall (disjoin #'zerop #'oddp) 1) ;; => T
(funcall (disjoin #'zerop #'oddp) 2) ;; => NIL
```

#### <span id="conjoin"> conjoin (predicate &rest more-predicates) </span>
Alias of `alexandria:disjoin`, returns a function that applies each of
`predicate` and `more-predicate` functions in turn to its arguments,
returning `NIL` if any of the predicates returns false, without
calling the remaining predicates. If none of the predicates returns
false, returns the primary value of the last predicate.

Examples:
```cl
(funcall (conjoin #'zerop #'evenp) 0) ;; => T
(funcall (conjoin #'zerop #'evenp) 1) ;; => NIL
(funcall (conjoin #'zerop #'evenp) 2) ;; => NIL
```

#### <span id="compose"> compose (function &rest more-functions) </span>
Alias of `alexandria:compose`, returns a function composed of
`function` and `more-functions` that applies its arguments to to each
in turn, starting from the rightmost of more-functions, and then
calling the next one with the primary value of the last.

Examples:
```cl
(funcall (compose #'exp #'1+) 1) ;; => exp (1+1) = 7.389056
(funcall (compose #'1+ #'exp) 1) ;; => 1+exp(1) = 3.7182817
```

#### <span id="curry"> curry (function &rest arguments) </span>
Alias of `alexandria:curry`, returns a function that applies
`arguments` and the arguments it is called with to `function`.

Examples:
```cl
(funcall (curry #'list 'a) 'b) ;; => (A B)
(funcall (curry #'list 'a 'b) 'c) ;; => (A B C)
```

#### <span id="rcurry"> rcurry (function &rest arguments) </span>
Alias of `alexandria:rcurry`, returns a function that applies the
`arguments` it is called with and arguments to `function`.

Examples:
```cl
(funcall (rcurry #'list 'a) 'b) ;; => (B A)
(funcall (rcurry #'list 'a 'b) 'c) ;; => (C A B)
```
