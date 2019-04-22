# utility
[![Version Badge](https://img.shields.io/badge/version-1.3-brightgreen.svg)](https://github.com/physcrf/utility)

My utility collection

## Introduction
There are serveral external generic utility libraries, such as famous
[Alexandria](https://common-lisp.net/project/alexandria/), in Common
Lisp world. However, usually `Alexandria` alone is not enough for
programing, and one need to import other libraries such as the de
facto standard utility
[uiop](https://common-lisp.net/project/asdf/uiop.html) or a large
supplement to `Alexandria`
[serapeum](https://github.com/ruricolist/serapeum). At the same time,
there may be name conflicts between different packages and one
need to choose the symbol by hand.

Therefore I just collect functions from multiple libraries into one
single package for my own use. Some functions are also renamed or
rewrapped to fit my convention. Also, there are some functions written by
my own. Right now, libraries dependencies are
- [Alexandria](https://common-lisp.net/project/alexandria/)
- [serapeum](https://github.com/ruricolist/serapeum)
- [uiop](https://common-lisp.net/project/asdf/uiop.html)
- [parse-number](https://github.com/sharplispers/parse-number)
- [parse-float](https://github.com/soemraws/parse-float)
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
- [copy-array](#copy-array)
### [control flow](#control-flow-ref)
- [switch](#switch)
- [cswitch](#cswitch)
- [eswitch](#eswitch)
- [select](#select)
- [select*](#select*)
- [eq*](#eq*)
- [eql*](#eql*)
- [equal*](#equal*)
- [equalp*](#equalp*)
### [function](#function-ref)
- [defalias](#defalias)
- [disjoin](#disjoin)
- [conjoin](#conjoin)
- [compose](#compose)
- [curry](#curry)
- [rcurry](#rcurry)
- [nested-loop](#nested-loop)
- [nested-map](#nested-map)
### [hash table](#hash-table-ref)
- [dict](#dict)
- [dict*](#dict*)
- [do-hash-table](#do-hash-table)
- [copy-hash-table](#copy-hash-table)
- [hash-table-keys](#hash-table-keys)
- [hash-table-values](#hash-table-values)
- [hash-table-alist](#hash-table-alist)
- [hash-table-plist](#hash-table-plist)
- [alist-hash-table](#alist-hash-table)
- [plist-hash-table](#plist-hash-table)
### [list](#list-ref)
- [appendf](#appendf)
- [append1](#append1)
- [lastcar](#lastcar)
- [plist-keys](#plist-keys)
- [plist-values](#plist-values)
- [insert](#insert)
### [macro](#macro-ref)
- [with-gensyms](#with-gensyms)
### [number](#number-ref)
- [parse-float](#parse-float)
- [parse-number](#parse-number)
- [parse-real-number](#parse-real-number)
- [parse-positive-real-number](#parse-positive-real-number)
- [bits](#bits)
- [unbits](#unbits)
- [random-in-range](#random-in-range)
- [iota](#iota)
- [string-integerp](#string-integerp)
- [string-floatp](#string-floatp)
- [string-realp](#string-realp)
- [string-complexp](#string-complexp)
- [string-numberp](#string-numberp)
- [square](#square)
- [cube](#cube)
### [sequence](#sequence-ref)
- [emptyp](#emptyp)
- [rotate](#rotate)
- [random-elt](#random-elt)
- [copy-sequence](#copy-sequence)
- [first-elt](#first-elt)
- [second-elt](#second-elt)
- [third-elt](#third-elt)
- [last-elt](#last-elt)
- [split-sequence](#split-sequence)
- [split-sequence-if](#split-sequence-if)
- [split-sequence-if-not](#split-sequence-if-not)
- [runs](#run)
- [batches](#batch)
- [frequencies](#frequencies)
- [assort](#assort)
- [partition](#partition)
- [do-each](#do-each)
- [filter](#filter)
- [keep](#keep)
- [single](#single)
- [cumulate](#cumulate)
- [of-length](#of-length)
- [length=](#length=)
- [length>](#length>)
- [length<](#length<)
- [length>=](#length>=)
- [length<=](#length<=)
- [longer](#longer)
- [longest](#longest)
- [take](#take)
- [drop](#drop)
- [remove-nth](#remove-nth)
### [stream](#stream-ref)
- [read-file-form](#read-file-form)
- [read-file-forms](#read-file-forms)
- [read-file-line](#read-file-line)
- [read-file-lines](#read-file-lines)
- [read-file-string](#read-file-string)
- [read-file-data](#read-file-data)
- [write-file-data](#write-file-data)
- [with-input-file](#with-input-file)
- [with-output-file](#with-output-file)
### [symbols](#symbols-ref)
- [make-keyworkd](#make-keyworkd)
- [symbolicate](#symbolicate)
- [find-keyword](#find-keyword)
### [types](#types-ref)
- [true](#true)
## Function Reference
### <span id="array-ref"> array </span>
#### <span id="indexes-to-row-major-index"> [Function] indexes-to-row-major-index (dimensions &rest subscripts) </span>
This function is written in reference to
[cffi](https://common-lisp.net/project/cffi/)'s internal utilities
(which are not exported by `cffi`). It transforms `subscripts` into a
row major index with respect to `dimensions`.

Examples:
```cl
(indexes-to-row-major-index '(4 5) 2 1) ;; => 11
(indexes-to-row-major-index '(2 2) 1 0) ;; => 2
```

#### <span id="row-major-index-to-indexes"> [function] row-major-index-to-indexes (index dimensions) </span>
Transforms a row major `index` into subscripts with respect to
`dimensions`.

Examples:
```cl
(row-major-index-to-indexes 2 '(2 2)) ;; => (1 0)
(row-major-index-to-indexes 11 '(4 5)) ;; => (2 1)
```

#### <span id="indexes-to-column-major-index"> [function] indexes-to-column-major-index (dimensions &rest subscripts) </span>
Transforms `subscripts` into a column major index with respect to
`dimensions`.

Examples:
```cl
(indexes-to-column-major-index '(4 5) 2 1) ;; => 6
(indexes-to-column-major-index '(2 2) 1 0) ;; => 1
```

#### <span id="column-major-index-to-indexes"> [function] column-major-index-to-indexes (index dimensions) </span>
Transforms a column major `index` into subscripts with respect to `dimensions`.

Examples:
```cl
(column-major-index-to-indexes 6 '(4 5)) ;; => (2 1)
(column-major-index-to-indexes 1 '(2 2)) ;; => (1 0)
```
#### <span id="copy-array"> [function] copy-array (array &key element-type fill-pointer adjustable) </span>
Alias of `alexandria:copy-array`, returns an undisplaced copy of
`array`, with same `fill-pointer` and `adjustability` (if any) as the
original, unless overridden by the keyword arguments.
### <span id="control-flow-ref"> control flow </span>
#### <span id="switch"> [macro] switch ((object &key test key) &body clauses) </span>
Alias of `alexandria:switch`, evaluates first matching clause,
returning its values, or evaluates and returns the values of default
if no keys match.

Examples:
```cl
(switch ("A" :test #'equal)
	   ("a" 1)
	   ("A" 2)
	   (t t)) ;; => 2

```


#### <span id="cswitch"> [macro] cswitch ((object &key test key) &body clauses) </span>
Alias of `alexandria:cswitch`, like [switch](#switch), but signals a
continuable error if no key matches.
#### <span id="eswitch"> [macro] eswitch ((object &key test key) &body clauses) </span>
Alias of `alexandria:cswitch`, like [switch](#switch), but signals an
error if no key matches.
#### <span id="select"> [macro] select (keyform &body clauses) </span>
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

#### <span id="select*"> [macro] select* (keyform test &body clauses) </span>
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

#### <span id="eq*"> [function] eq* (&rest objects) </span>
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

#### <span id="eql*"> [function] eql* (&rest objects) </span>
Alias of `serapeum:eql*`, variadic version of `cl:eql`. Usage is the
same as [`eq*`](#eq*) except using `cl:eql` to compare.

#### <span id="equal*"> [function] equal* (&rest objects) </span>
Alias of `serapeum:equal*`, variadic version of `cl:equal`. Usage is the
same as [`eq*`](#eq*) except using `cl:equal` to compare.

#### <span id="equalp*"> [function] equalp* (&rest objects) </span>
Alias of `serapeum:equalp*`, variadic version of `cl:equalp`. Usage is
the same as [`eq*`](#eq*) except using `cl:equalp` to compare.


### <span id="function-ref"> function </span>
#### <span id="defalias"> [function] defalias (alias macro-or-function &optional docstring) </span>
Defines alias of `macro-or-function` with optional `docstring`. If
`docstring` is not `NIL`, the old documentation would be replaced by
`docstring`.

Examples:
```cl
(defalias 'my-sin 'sin) ;; => MY-SIN
(my-sin 1) ;; => 0.84147096
(defalias 'my-sin 'sin "My sine") ;; => MY-SIN
(documentation 'my-sin 'function) ;; => "My sine"
```
#### <span id="disjoin"> [function] disjoin (predicate &rest more-predicates) </span>
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

#### <span id="conjoin"> [function] conjoin (predicate &rest more-predicates) </span>
Alias of `alexandria:conjoin`, returns a function that applies each of
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

#### <span id="compose"> [function] compose (function &rest more-functions) </span>
Alias of `alexandria:compose`, returns a function composed of
`function` and `more-functions` that applies its arguments to to each
in turn, starting from the rightmost of more-functions, and then
calling the next one with the primary value of the last.

Examples:
```cl
(funcall (compose #'exp #'1+) 1) ;; => exp (1+1) = 7.389056
(funcall (compose #'1+ #'exp) 1) ;; => 1+exp(1) = 3.7182817
```

#### <span id="curry"> [function] curry (function &rest arguments) </span>
Alias of `alexandria:curry`, returns a function that applies
`arguments` and the arguments it is called with to `function`.

Examples:
```cl
(funcall (curry #'list 'a) 'b) ;; => (A B)
(funcall (curry #'list 'a 'b) 'c) ;; => (A B C)
```

#### <span id="rcurry"> [function] rcurry (function &rest arguments) </span>
Alias of `alexandria:rcurry`, returns a function that applies the
`arguments` it is called with and arguments to `function`.

Examples:
```cl
(funcall (rcurry #'list 'a) 'b) ;; => (B A)
(funcall (rcurry #'list 'a 'b) 'c) ;; => (C A B)
```

#### <span id="nested-loop"> [macro] nested-loop (subscripts dimensions &body body) </span>
Borrowed from [huaiyuan's answer on
stackoverflow](https://stackoverflow.com/questions/10163298/lisp-macro-or-function-for-nested-loops).
This macro do nested loop over dimensions, see examples.

Examples:
```cl
(nested-loop (x y) (2 2)
	(format t "~A ~A~%" x y))
;; => 
0 0
0 1
1 0
1 1
```

#### <span id="nested-map"> [function] nested-map (dimensions function) </span>
If the dimensions cannot be decided at compile time, for instance we
want print elements of an array whose dimensions is not known yet,
then we need a function. This part is also borrowed from [huaiyuan's
answer on
stackoverflow](https://stackoverflow.com/questions/10163298/lisp-macro-or-function-for-nested-loops).

Examples:
```cl
(nested-map '(2 2) 
	(lambda (&rest arguments) (print arguments)))
;; =>
(0 0) 
(0 1) 
(1 0) 
(1 1) 
```

### <span id="hash-table-ref"> hash table </span>
#### <span id="dict"> [function] dict (keys-and-values) </span>
Alias of `serapeum:dict`, a concise constructor for hash tables.

Examples:
```cl
(gethash :c (dict :a 1 :b 2 :c 3)) ;; => 3, T
```

#### <span id="dict*"> [function] dict* (dict &rest keys-and-values) </span>
Alias of `serapeum:dict*`, merges new bindings into `dict`. 

Examples:
```cl
(defparameter table (dict :a 1 :b 2 :c 3))
table ;; => #<HASH-TABLE :TEST EQUAL :COUNT 3>
(dict* table :d 4)
table ;; => #<HASH-TABLE :TEST EQUAL :COUNT 4>
```

#### <span id="do-hash-table"> [macro] do-hash-table ((key value table &optional return) &body body) </span>
Alias of `serapeum:do-hash-table`, iterates over hash table `table` in
no particular order.

Examples:
```cl
(let ((table (dict :a 1 :b 2 :c 3)))
	   (do-hash-table (key value table)
	     (print (list key value))))
;; => 
(:A 1) 
(:B 2) 
(:C 3) 
```

#### <span id="copy-hash-table"> [function] copy-hash-table (table &key key test size rehash-size rehash-threshold) </span>
Alias of `alexandria:copy-hash-table`, returns a copy of hash table
table, with the same keys and values as the table. The copy has the
same properties as the original, unless overridden by the keyword
arguments.

Before each of the original values is set into the new hash-table, key
is invoked on the value. As key defaults to `cl:identity`, a shallow
copy is returned by default.

#### <span id="hash-table-keys"> [function] hash-table-keys (table) </span>
Alias of `alexandria:hash-table-keys`, returns a list containing the
keys of hash table `table`.

Examples:
```cl
(let ((table (dict :a 1 :b 2 :c 3)))
	   (hash-table-keys table)) 
;; => (:C :B :A)
```

#### <span id="hash-table-values"> [function] hash-table-values (table) </span>
Alias of `alexandria:hash-table-values`, returns a list containing the
values of hash table `table`.

Examples:
```cl
(let ((table (dict :a 1 :b 2 :c 3)))
	   (hash-table-values table))
;; => (3 2 1)
```

#### <span id="hash-table-alist"> [function] hash-table-alist (table) </span>
Alias of `alexandria:hash-table-alist`, returns an association list
containing the keys and values of hash table `table`.

Examples:
```cl
(let ((table (dict :a 1 :b 2 :c 3)))
	    (hash-table-alist table))
;; => ((:C . 3) (:B . 2) (:A . 1))
```

#### <span id="hash-table-plist"> [function] hash-table-plist (table) </span>
Alias of `alexandria:hash-table-plist`, returns a property list
containing the keys and values of hash table `table`.

Examples:
```cl
(let ((table (dict :a 1 :b 2 :c 3)))
	   (hash-table-plist table))
;; => (:C 3 :B 2 :A 1)
```

#### <span id="alist-hash-table"> [function] alist-hash-table (alist &rest hash-table-initargs) </span>
Alias of `alexandria:alist-hash-table`, returns a hash table
containing the keys and values of the association list `alist`. Hash
table is initialized using the `hash-table-initargs`.

Examples:
```cl
(alist-hash-table '((:C . 3) (:B . 2) (:A . 1)))
;; => #<HASH-TABLE :TEST EQL :COUNT 3>
```

#### <span id="plist-hash-table"> [function] plist-hash-table (plist &rest hash-table-initargs) </span>
Alias of `alexandria:plist-hash-table`, returns a hash table
containing the keys and values of the property list `plist`. Hash table
is initialized using the `hash-table-initargs`.

Examples:
```cl
(plist-hash-table '(:C 3 :B 2 :A 1))
;; => #<HASH-TABLE :TEST EQL :COUNT 3>
```



### <span id="list-ref"> list </span>
#### <span id="appendf"> [macro] appendf (place &rest lists) </span>
Alias of `alexandria:appendf`, modify-macro for `cl:append`. Appends
`lists` to the `place` designated by the first argument.

Examples:
```cl
(defparameter a '(1 2))
(appendf a '(3 4) '(5 6))
a ;; => (1 2 3 4 5 6)
```

#### <span id="append1"> [function] append1 (list item) </span>
Alias of `serapeum:append1`, append an atom `item` to a list `list`.

Examples:
```cl
(append1 '(1 2) 3) ;; => (1 2 3)
```

#### <span id="lastcar"> [function] lastcar (list) </span>
Alias of `alexandria:lastcar`, returns the last element of
`list`. Signals a type-error if `list` is not a proper list.
It is setfable.

Examples:
```cl
(lastcar '(1 2 3)) ;; => 3
```

#### <span id="plist-keys"> [function] plist-keys (plist) </span>
Alias of `serapeum:plist-keys`, returns the keys of `plist`.

Examples:
```cl
(plist-keys '(:a 1 :b 2 :c 3)) ;; => (:A :B :C)
```

#### <span id="plist-values"> [function] plist-values (plist) </span>
Alias of `serapeum:plist-values`, returns the values of `plist`.

Examples:
```cl
(plist-values '(:a 1 :b 2 :c 3)) ;; => (1 2 3)
```

#### <span id="insert"> [macro] insert (position object list) </span>
Inserts `object` into `list` at `position`.

Examples:
```cl
(defparameter a '(1 3))
(insert 1 2 a)
a ;; => (1 2 3)
```
### <span id="macro-ref"> macro </span>
#### <span id="with-gensyms"> [macro] with-gensyms (names &body) </span>
Alias of `alexandria:with-gensyms`, binds each variable named by a
symbol in names to a unique symbol around forms. Each of names must
either be either a symbol, or of the form:

```cl
(symbol string-designator)
```

Bare symbols appearing in names are equivalent to:

```cl
(symbol symbol)
```

The string-designator is used as the argument to gensym when
constructing the unique symbol the named variable will be bound to.

### <span id="number-ref"> number </span>
#### <span id="parse-float"> [function] parse-float (string &key start end radix junk-allowed decimal-character exponent-character type) </span>
Alias of `parse-float:parse-float`, detailed documentation refers to
[parse-float](https://github.com/soemraws/parse-float).

Examples:
```cl
(parse-float "1.23") ;; => 1.23
(parse-float "1") ;; => 1.0
```
#### <span id="parse-number"> [function] parse-number (string &key start end radix) </span>
Alias of `parse-number:parse-number`, parses `string` into number.
See [parse-number](http://cliki.net/parse-number).

Examples:
```cl
(parse-number "123") ;; => 123
(parse-number "123" :start 1) ;; => 23
(parse-number "123" :end 1) ;; => 1
(parse-number "123" :radix 4) ;; => 1x4**2 + 2x4 + 3 = 27
(parse-number "12.3") ;; => 12.3
(parse-number "1.23e1") ;; => 12.3
(parse-number "1.23d1") ;; => 12.3d0
(parse-number "-12.3") ;; -12.3
(parse-number "#C(12 3)") ;; => C(12 3)
(parse-number "#C(12.0 3)") ;; => C(12.0 3.0)
```

#### <span id="parse-real-number"> [function] parse-real-number (string &key start end radix) </span>
Alias of `parse-number:parse-real-number`, parses `string` into real
number.  Like [parse-number](#parse-number), but it will signal an
error when encounter complex number.

#### <span id="parse-positive-real-number"> [function] parse-positive-real-number (string &key start end radix) </span>
Alias of `parse-number:parse-positive-real-number`, parses `string`
into positive real number. Like [parse-number](#parse-number), but it
will signal an error when encounter complex and negative number.

#### <span id="bits"> [function] bits (int &key big-endian) </span>
Alias of `serapeum:bits`, returns a bit vector of the bits in
`int`. Defaults to little-endian.

Examples:
```cl
(bits 4) ;; => #*001
(bits 4 :big-endian t) ;; => #*100
```

#### <span id="unbits"> [function] unbits (bits &key big-endian) </span>
Alias of `serapeum:unbits`, turns a bit vector `bits` into an
integer. Defaults to little-endian.

Examples:
```cl
(unbits #*001) ;; => 4
(unbits #*001 :big-endian t) ;; => 1
(unbits #*100 :big-endian t) ;; => 4
```

#### <span id="random-in-range"> [function] random-in-range (low high) </span>
Alias of `serapeum:random-in-range`, random number in the range
[`low`, `high`). `low` and `high` are automatically swapped if `high`
is less than `low`.
#### <span id="iota"> [function] iota (n &key (start 0) (start 1)) </span>
Alias of `alexandria:iota`, returns a list of `n` numbers, starting
from `start`, each consequtive number being the sum of the previous
one and `step`. start defaults to 0 and step to 1.

Examples:
```cl
(iota 4) ;; => (0 1 2 3)
(iota 3 :start 1 :step 1.0) ;; => (1.0 2.0 3.0)
(iota 3 :start -1 :step -1/2) ;; => (-1 -3/2 -2)
```
#### <span id="string-integerp"> [function] string-integerp (string) </span>
Tells if a string `string` represents an integer. This function tries to
be compatible with `cl:integerp`, therefore string like "1." is
treated as integer.

Examples:
```cl
(string-integerp "1") ;; => T
(cl:integerp 1.) ;; => T
(string-integerp "1.") ;; => T
(string-integerp "1.0") ;; => NIL
```

#### <span id="string-floatp"> [function] string-floatp (string) </span>
Tells if a string `string` represents a float number.

Examples:
```cl
(string-floatp "1") ;; => NIL
(string-floatp "1.") ;; => NIL
(string-floatp "1.0) ;; => T
```

#### <span id="string-realp"> [function] string-realp (string) </span>
Tells if a string `string` represents a real number.

Examples:
```cl
(string-realp "-1.23E4") ;; => T
(string-realp "-1.2.3E4") ;; => NIL
```

#### <span id="string-complexp"> [function] string-complexp (string) </span>
Tells if a string `string` represents a real number. 

Note that this function tries to be compatible with `cl:complexp`,
thus string like "#C(1 0)" would be treated as a real number rather
than a complex number.

```Examples:
(string-complexp "1") ;; => NIL
(string-complexp "1.2") ;; => NIL
(string-complexp "#C(1 0)") ;; => NIL
(string-complexp "#C(1 0.0)") ;; => T
(string-complexp "#C(1 1)") ;; => T
```

#### <span id="string-numberp"> [function] string-numberp (string) </span>
Tells if a string `string` represents a number. If
[string-realp](#string-realp) or [string-complexp](#string-complexp)
returns true, then it is a number.

#### <span id="square"> [function] square (x) </span>
Returns the square of `x`, `x**2`.
#### <span id="cube"> [function] cube (x) </span>
Returns the cube of `x`, `x**3`.
### <span id="sequence-ref"> sequence </span>
#### <span id="emptyp"> [generic function] emptyp (sequence) </span>
Alias of `alexandria:emptyp`, returns true if `sequence` is an empty
sequence. Signals an error if `sequence` is not a sequence.

#### <span id="rotate"> [function] rotate (sequence &optional (n 1)) </span>
Alias of `alexandria:rotate`, returns a sequence of the same type as
`sequence`, with the elements of sequence rotated by `n`. `n`must be
an integer.

Note: the original sequence may be destructively altered, and result
sequence may share structure with it.

Examples:
```cl
(rotate '(1 2 3)) ;; => (3 1 2)
(rotate #(1 2 3)) ;; => #(3 1 2)
```

#### <span id="random-elt"> [function] random-elt (sequence &key (start 0) end) </span>
Alias of `alexandria:random-elt`, returns a random element from
`sequence` bounded by `start` and `end`. Signals an error if the
sequence is not a proper non-empty sequence, or if `end` and `start`
are not proper bounding index designators for sequence.

#### <span id="copy-sequence"> [function] copy-sequence (type sequence) </span>
Alias of `alexandria:copy-sequence`, returns a fresh sequence of `type`,
which has the same elements as `sequence`.

#### <span id="first-elt"> [function] first-elt (sequence) </span>
Alias of `alexandria:first-elt`, returns the first element of
`sequence`. Signals a type-error if `sequence` is not a sequence, or
is an empty sequence. It is setfable.

#### <span id="second-elt"> [function] second-elt (sequence) </span>
Returns the second element of `sequence`. Signals a type-error if
`sequence` is not a sequence, or it is an empty sequence. It is setfable.

#### <span id="third-elt"> [function] third-elt (sequence) </span>
Returns the third element of `sequence`. Signals a type-error if
`sequence` is not a sequence, or it is an empty sequence. It is setfable.

#### <span id="last-elt"> [function] last-elt (sequence) </span>
Alias of `alexandria:last-elt`, returns the last element of
`sequence`. Signals a type-error if `sequence` is not a proper
sequence, or is an empty sequence. It is setfable.

#### <span id="split-sequence"> [function] split-sequence (delimiter sequence &key count remove-empty-subseqs from-end (start 0) end (test #'eql) test-not (key #'identity)) </span>
Alias of `split-sequence:split-sequence`, splits `sequence` into
sub-sequences according to `delimiter`. Detail documentation refers
to [split-sequence](https://www.cliki.net/SPLIT-SEQUENCE).

Examples:
```cl
(split-sequence :delimter '(a :delimter b :delimter c))
;; => ((A) (B) (C)), 5
```

#### <span id="split-sequence-if"> [function] split-sequence-if (predicate sequence &key count remove-empty-subseqs from-end (start 0) end (key #'identity)) </span>
Alias of `split-sequence:split-sequence-if`, splits `sequence` into 
sub-sequences according to `predicate`. Detail documentation refers
to [split-sequence](https://www.cliki.net/SPLIT-SEQUENCE).

Examples:
```cl
(split-sequence-if #'evenp '(1 2 3 4 5)) ;; => ((1) (3) (5)), 5
(split-sequence-if #'oddp '(1 2 3 4 5)) ;; => (NIL (2) (4) NIL), 5
```

#### <span id="split-sequence-if-not"> [function] split-sequence-if-not (predicate sequence &key count remove-empty-subseqs from-end (start 0) end (key #'identity)) </span>
Alias of `split-sequence:split-sequence-if-not`, splits `sequence`
into sub-sequences according to not `predicate`. Detail documentation
refers to [split-sequence](https://www.cliki.net/SPLIT-SEQUENCE).

Examples:
```cl
(split-sequence-if-not #'evenp '(1 2 3 4 5)) ;; => (NIL (2) (4) NIL), 5
(split-sequence-if-not #'oddp '(1 2 3 4 5)) ;; => ((1) (3) (5)), 5
```

#### <span id="runs"> [function] runs (sequence &key (start 0) end (key #'identity) (test #'eql)) </span>
Alias of `serapeum:runs`, returns a list of runs of similar elements
in `sequence`. The arguments `start`, `end`, and `key` are as for
`cl:reduce`.

Examples:
```cl
(runs '(head tail head head tail)) ;; => ((HEAD) (TAIL) (HEAD HEAD) (TAIL))
(runs #(head tail head head tail)) ;; => (#(HEAD) #(TAIL) #(HEAD HEAD) #(TAIL))
```

#### <span id="batches">  [function] batches (sequence n &key (start 0) end even) </span>
Alias of `serapeum:batches`, return `sequence` in batches of `n`
elements. If `even` is true, the sequence must be evenly divided
otherwise an error is signaled.

Examples:
```cl
(batches '(0 1 2 3 4 5 6 7 8 9 10) 2) ;; => ((0 1) (2 3) (4 5) (6 7) (8 9) (10))
(batches #(0 1 2 3 4 5 6 7 8 9 10) 2) ;; => (#(0 1) #(2 3) #(4 5) #(6 7) #(8 9) #(10))
```

#### <span id="frequencies">  [function] frequencies (sequence &rest hash-table-args &key key &allow-other-keys) </span>
Alias of `serapeum:frequencies`, returns a hash table with the count
of each unique item in `sequence`. As a second value, return the length of
`sequence`.

Examples:
```cl
(frequencies '(1 2 2 3 3 3)) ;; => #<HASH-TABLE :TEST EQUAL :COUNT 3>, 6
(frequencies #(1 2 2 3 3 3)) ;; => #<HASH-TABLE :TEST EQUAL :COUNT 3>, 6
```

#### <span id="assort">  [function] assort (sequence &key (key #'identity) (test #'eql) (start 0) end) </span>
Alias of `serapeum:assort`, returns `sequence` assorted by
`key`. *Seems this function has a bug*.

Examples:
```cl
(assort '(0 1 2 3 4 5 6 7 8 9 10) :key #'evenp) 
;; => ((0 2 4 6 8 10) (1 3 5 7 9))
```

#### <span id="partition">  [function] partition (predicate sequence &key (start 0) end (key #'identity)) </span>
Alias of `serapeum:partition`, partition elements of `sequence` into
those for which `predicate` returns true and false. Returns two
values, one with each sequence.

Note: `partition` is not just `assort` with an up-or-down
predicate. `assort` returns its groupings in the order they occur in
the sequence; `partition` always returns the true elements first.

Examples:
```cl
(partition #'evenp '(1 2 3)) ;; => (2), (1 3)
(partition #'evenp #(1 2 3)) ;; => #(2), #(1 3)
```

#### <span id="do-each"> [macro] do-each ((var sequence &optional return) &body body) </span>
Alias of `serapeum:do-each`, iterates over the elements of `sequence`,
a sequence. If `sequence` is a list, this is equivalent to
`cl:dolist`.

Examples:
```cl
(do-each (x #(1 2 3 4))
	(print x))
;; => 
1
2
3
4
```

#### <span id="filter"> [function] filter (predicate sequence &rest args &key count &allow-other-keys) </span>
Alias of `serapeum:filter`, almost but not quite an alias of `cl:remove-if-not`.

The difference is the handling of `count`: for `filter`, `count` is
the number of items to keep, not remove.

Examples:
```cl
(remove-if-not #'oddp '(1 2 3 4 5 6) :count 2) ;; => (1 3 5 6)

(filter #'oddp '(1 2 3 4 5 6) :count 2) ;;  => (1 3)
 ```
 
#### <span id="keep"> [function] keep (item sequence &rest args &key (test #'eql) from-end key count &allow-other-keys) </span>
Alias of `serapeum:keep`, alias but not quite an alias of `cl:remove`
with `:test-not` instead of `:test`.

The difference is the handling of `count`. For `keep`, `count` is the
number of items to keep, not remove.

Examples:
```cl
(remove 'x '(x y x y x y) :count 2) ;; => (Y Y X Y)
(keep 'x '(x y x y x y) :count 2) ;; => (X X)
```

`keep` becomes useful with the `key` argument:
```cl
(keep 'x '((x 1) (y 2) (x 3)) :key #'car) ;; => ((X 1) (X 3))
```

#### <span id="single"> [function] single (sequence) </span>
Alias of `serapeum:single`, tests if `sequence` has only one element.

Examples:
```cl
(single #()) ;; => NIL
(single #(1)) ;; => T
(single #(1 2)) ;; => NIL
```

#### <span id="cumulate"> [function] cumulate (func sequence &rest args &key from-end (start 0) end initial-values &allow-other-keys) </span>
Alias of `serapeum:scan`, returns the partial reductions of `sequence`.

Each element of the result sequence is the result of calling
`cl:reduce` on the elements of the `sequence` up to that point
(inclusively).

Examples:
```cl
(reduce #'+ '(1))       ;; => 1
(reduce #'+ '(1 2))     ;; => 3
(reduce #'+ '(1 2 3))   ;; => 6
(reduce #'+ '(1 2 3 4)) ;; => 10
(cumulate   #'+ '(1 2 3 4)) ;; => (1 3 6 10)
```

The result of calling `cumulate` on an empty sequence is always an
empty sequence, however.
```cl
(reduce #'+ '()) ;; => 0
(cumulate #'+ '()) ;; => NIL
```

#### <span id="of-length"> [function] of-length (n sequence) </span>
Checks if the length of `sequence` is `n`.

#### <span id="length="> [function] length= (&rest sequences) </span>
Alias of `alexandria:length=`, takes any number of sequences or
integers in any order. Returns true if the length of all the
sequences and the integers are equal.

#### <span id="length>"> [function] length> (&rest sequences) </span>
Alias of `serapeum:length>`, checks if each length designator in
`sequences` longer than the next. A length designator may be a
sequence or an integer.

#### <span id="length<"> [function] length< (&rest sequences) </span>
Alias of `serapeum:length<`, checks if each length designator in
`sequences` shorter than the next. A length designator may be a
sequence or an integer.

#### <span id="length>="> [function] length>= (&rest sequences) </span>
Alias of `serapeum:length>=`, checks if each length designator in
`sequences` not shorter than the next. A length designator may be a
sequence or an integer.

#### <span id="length<="> [function] length<= (&rest sequences) </span>
Alias of `serapeum:length<=`, checks if each length designator in
`sequences` not longer than the next. A length designator may be a
sequence or an integer.

#### <span id="longer"> [function] longer (x y) </span>
Alias of `serapeum:longer`, returns the longer of `x` and `y`.
If `x` and `y` are of equal length, then return `x`.

#### <span id="longest"> [function] longest (&rest sequences) </span>
Wrapper of `serapeum:longest`, returns the longest sequence in
`sequences`.


#### <span id="take"> [function] take (n sequence) </span>
Alias of `serapeum:take`, returns at most the first `n` elements of
`sequence`. New sequence is of same type of `sequence`.

If `n` is larger than length of `sequence`, simply copy `sequence`.

If `n` is negative, then the first |`n`| elements are taken.

#### <span id="drop"> [function] drop (n sequence) </span>
Alias of `serapeum:drop`, returns all but first `n` elements in
`sequence`. New sequence is of same type of `sequence`.

If `n` is larger than the length of `sequence`, returns an empty
sequence.

If `n` is negative, then |`n`| elements are dropped.

#### <span id="remove-nth"> [function] remove-nth (n sequence) </span>
Returns a sequence without `n`th element in `sequence`.

Examples:
```cl
(defparameter a #(1 2 3 4)) 
(remove-nth 2 a) ;; => #(1 2 4)
```

### <span id="stream-ref"> stream </span>
#### <span id="read-file-form"> [function] read-file-form (file &rest keys &key (at 0) &allow-other-keys) </span>
Alias of `uiop:read-file-form`, opens input `file` with option `keys`
(except `at`), and read its contents as per `uiop:slurp-stream-form`
with given at specifier. beware: be sure to use `uiop:with-safe-io-syntax`,
or some variant thereof.

#### <span id="read-file-forms"> [function] read-file-forms (file &rest keys &key count &allow-other-keys) </span>
Alias of `uiop:read-file-forms`, opens input `file` with option `keys`
(except `count`), and read its contents as per
`uiop:slurp-stream-forms` with given `count`. beware: be sure to use
`uiop:with-safe-io-syntax`, or some variant thereof.

#### <span id="read-file-line"> [function] read-file-line (file &rest keys &key (at 0) &allow-other-keys) </span>
Alias of `uiop:read-file-line`, opens input `file` with option `keys`
(except `at`), and read its contents as per `uiop:slurp-stream-line`
with given at specifier. beware: be sure to use
`uiop:with-safe-io-syntax`, or some variant thereof.

#### <span id="read-file-lines"> [function] read-file-lines (file &rest keys &key count &allow-other-keys) </span>
Alias of `uiop:read-file-lines`, opens input `file` with option
`keys`, read its contents as a list of lines. beware: be sure to use
`uiop:with-safe-io-syntax`, or some variant thereof.

#### <span id="read-file-string"> [function] read-file-string  (file &rest keys) </span>
Alias of `uiop:read-file-string`, open input `file` with option
`keys`, read its contents as a string.




#### <span id="read-file-data"> [function] read-file-data (filename &key (comments "#") (delimiter "\\s+")) </span>
Reads data from file `filename` into a nested (2 dimensional) list.
#### <span id="write-file-data"> [function] write-file-data (filename data &key (format "~10,8G") (delimiter "~8T") (comments "#") (header "") (footer "")) </span>
Writes data in a nested (2 dimensional) list into file `filename`.
#### <span id="with-input-file"> with-input-file ((stream filespec) &body body) </span>
A shorthand of `cl:with-open-file` with `(:direction :input)`.

#### <span id="with-output-file">  with-output-file ((stream filespec) &body body) </span>
A shorthand of `cl:with-open-file` with `(:direction :output :if-exists :supersede :if-does-not-exist :create)`.
### <span id="symbol-ref"> symbol </span>
#### <span id="make-keyword"> [function] make-keyword (name) </span>
Alias of `alexandria:make-keyword`, interns the string designated by
`name` in the keyword package.

Examples:
```cl
(make-keyword "HELLO") ;; => :HELLO
(make-keyword "hello") ;; => :|hello|
```

#### <span id="symbolicate"> [function] symbolicate (&rest things) </span>
Alias of `alexandria:symbolicate`, concatenates together the names of
some strings and symbols, producing a symbol in the current package.

Examples:
```cl
(symbolicate "HELLO") ;; => HELLO
(symbolicate "HELLO" '- 'world) ;; => HELLO-WORLD
```

#### <span id="find-keyword"> [function] find-keyword (string) </span>
Alias of `serapeum:find-keyword`, if `string` has been interned as a
keyword, return it.

Examples:
```cl
(make-keyword "HELLO")

(find-keyword "HELLO") ;; => :HELLO, :EXTERNAL
(find-keyword "WORLD") ;; => NIL, NIL
```

### <span id="types-ref"> types </span>
#### <span id="true"> [function] true (x) </span>
Alias of `serapeum:true`, if `x` is `NIL`, return `NIL`, otherwise return `T`.

