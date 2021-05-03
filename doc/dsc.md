<!--
 Copyright (c) 2020,21 EPITA Research and Development Laboratory

 Permission is hereby granted, free of charge, to any person obtaining
 a copy of this software and associated documentation
 files (the "Software"), to deal in the Software without restriction,
 including without limitation the rights to use, copy, modify, merge,
 publish, distribute, sublicense, and/or sell copies of the Software,
 and to permit persons to whom the Software is furnished to do so,
 subject to the following conditions:

 The above copyright notice and this permission notice shall be
 included in all copies or substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-->

# `dsfn` and `dscase`

## dsfn

The `dsfn` macro can be thought of as a generalization of the clojure build-in `fn` macro.
However `dsfn` offers two features that `fn` does not:

1. [Runtime selection by structure](#runtime-selection-by-structure)
2. [Runtime selection by type](#runtime-selection-by-type)

### Runtime selection by structure
Whereas, `fn` throws an exception if the given value fails to match the format of the parameter list.
The following expression
```clojure
((fn [a [b c] d]
  12)
 1 2 3)
```
results in an error such as
```
Execution error (ArityException) at clojure-rte.core/eval41272 (form-init6569138664565782700.clj:12360).
Wrong number of args (1) passed to: clojure-rte.core/eval41272/fn--41274
```
But, `((dsfn [a [b c] d] 12) 1 2 3)`, simply returns `nil` as the data `(1 2 3)` 
does not match the template `[a [b c] d]`.

Similarly, `fn` provides a syntax for providing several argument templates, however the semantics
do not permit `fn` of choosing the correct template according to the shape of the data. E.g.,
you might expect (hope) in vain that the following `fn` would select `[a b [c d]]`, because it 
matches the format of the given argument `(1 2 [3 4])`
```clojure
((fn 
   ([[a b] c d]   12)
   ([a [b c] d]   13)
   ([a b [c d]]   14))
 1 2 [3 4])
```
But, alas it does not.   Rather you get an error such as the following:
```
Syntax error compiling fn* at (clojure-rte:localhost:58696(clj)*:12379:20).
Can't have 2 overloads with same arity
```
On the contrary the following expression evaluates to 14 as it matches the structure of `(1 2 (3 4))`
to the argument list `[a b [c d]]`.

```clojure
((dsfn
   ([[a b] c d]  12)
   ([a [b c] d]  13)
   ([a b [c d]]  14))
 1 2 [3 4])
```

### Runtime selection by type

Use `dsfn` to perform run-time format and type checking.
in addition to a structured argument list such as `[a [b c] d]`,
you may also specify type constraints, with as meta-data within
the argument list itself, or in the map specified after the
argument list.  Within the argument list you may only specify
very simply type constraints, i.e., symbols which designate types, such as `^Boolean` and `^Number`.
However in the map, you may specify Boolean combinations of any type designator
supported by `clojure-rte.type`.  See [Extensible types](genus.md) for more details.

```clojure
;; match if a is of type Number
((dsfn [^Number a [b c] d]
  12)
 1 [2 3] 4)
```

```clojure
;; match if a is of type Number and c is String
((dsfn [^Boolean a [b ^String c] d]
  12)
 true [2 "three"] 4)
```

The following expression returns `nil` because although `(1 2 (3 4))`
matches the structure `[a b [c d]]`, the type of `a` is not `Boolean`.

```clojure
((dsfn 
   ([[^Boolean a b] c d]  12)
   ([^Boolean a [b c] d] 13)
   ([^Boolean a b [c d]] 14))
 1 2 [3 4])
```

The following expression evaluates to 15.  Why? Because `(1 2 (3 4))` matches
`[^Number a b [c d]]` in type and structure.

```clojure
((dsfn 
   ([[^Boolean a b] c d]  12)
   ([^Boolean a [b c] d] 13)
   ([^Boolean a b [c d]] 14)
   ([^Number  a b [c d]] 15))
 1 2 [3 4])
```

A map, `{...}`, may be used to specify a map from variable name to type constraint.
The following two clauses are equivalent `[^Boolean a [b ^String c] d]` 
and `^{a Boolean c String} [a [b c] d]`.  With simple type specifiers, the two forms are equivalent.
However, the second form allows more flexible type constraints such as the following.

- `^{[a b] Boolean c String}   [a [b c] d]` --- `a` and `b` are both `Boolean`, and `c` is String.
- `^{a  Boolean b (or Boolean String) c String}   [a [b c] d]` --- `a` is `Boolean`, `c` is String, and `b` is either `Boolean` or `String`.
- `^{a Boolean b (and Number (not Long))}  [a [b c] d]` --- `a` is `Boolean`, and `b` is either `Number` but not `Long`.
- `^{a (member -1 0 1) b (and (Number (not (= 0))))}   [a b c]` --- `a` is -1, 0 or 1, and `b` is a `Number` different from 0.


## destructuring using keys

A syntax such as the following can be used to match optional keyword arguments.

```clojure
((dsfn 
   ([a b & {:keys [foo bar]}]  12) ;; same as :allow-other-keys false
   ([a b & {:keys [foo bar] :allow-other-keys true}]   13)
   ([a b & {:keys [foo bar] :or {a 1 b 2}}]             15)
   ...)
 ...)
```

There are some subtle semantics about keys.  For example if
`:allow-other-keys` is not specified or if it is specified to be
`false`, then the clause will NOT match if the call-site has a key
which is not mentioned.  By *not mentioned* we man neither mentioned
in the defaults `:or` nor mentioned in the call-site argument list

If `:allow-other-keys true` is specified, then unmentioned keys will
have no effect on whether a clause matches.

If `:or` is given to specify default values, the semantics are
equivalent to add the corresponding keys to the call-site arguments if
they are missing.

### examples of dsfn using :keys

For the first example, we ignore type designators and default values.
If `:allow-other-keys false` is used (or if `:allow-other-keys` is completely omitted) in a
clause, then the clause is select which specifies exactly the given keys.

Suppose a function is defined as follows:

```clojure
(def g (dsfn 
         ([& {:keys [foo bar] :allow-other-keys false}]  12) 
         ([& {:keys [foo bar] :allow-other-keys true}]   13)
         ([& {:keys [foo] :allow-other-keys false}]      14)
         ))
```

- `(g :foo 1 :bar 2)` evaluates to 12, as the call-site specifies exactly the keys `:foo` and `:bar` and `:allow-other-keys` is `false`.
- `(g :foo 1 :bar 2 :baz 3)` evaluates to 13, as the call-site specifies a key  other than `:foo` and `:bar` and `:allow-other-keys` is true.
- `(g :foo 1)` evaluates to 14, because that is the first clause which specifies only key `:foo`.
- `(g :bar 1)` evaluates to `nil`, because every clause requires a `:foo` be given.
- `(g :foo 1 :baz 2)` evaluates to `nil`.   Neither 12 nor 13 is returned, as the call site does not include `:bar`.  14 is not returned because the call site contains `:baz`, but the pattern contains `:allow-other-keys false`.
- `(g)` evaluates to `nil` as no clause matches.  Each of the given clauses requires at least `:foo` at the call-site.


For the second example, we include some default values.

```clojure
(def g (dsfn 
         ([& {:keys [foo bar] 
              :or {foo 1}
              :allow-other-keys false}]  12) 
         ([& {:keys [foo bar] :allow-other-keys true}]   13)
         ([& {:keys [foo] :allow-other-keys false}]      14)
         ))
```

- `(g :foo 1 :bar 2)` evaluates to 12, as the call-site specifies exactly the keys `:foo` and `:bar` and `:allow-other-keys` is `false`.
- `(g :foo 1 :bar 2 :baz 3)` evaluates to 13, as the call-site specifies a key  other than `:foo` and `:bar` and `:allow-other-keys` is true.
- `(g :foo 1)` evaluates to 14, because that is the first clause which specifies only key `:foo`.
- `(g :bar 2)` evaluates to 12, because the first clause requires `:foo` and `:bar`.  The call-site provides the `:bar` and the defaults `:or {foo 1}` effectively provides the `:foo`.
- `(g :foo 1 :baz 2)` evaluates to `nil`.   Neither 12 nor 13 is returned, as the call site does not include `:bar`.  14 is not returned because the call site contains `:baz`, but the pattern contains `:allow-other-keys false`.
- `(g)` evaluates to `nil` as no clause matches.  Each of the given clauses requires at least `:foo` at the call-site.




## dscase

The `dscase` macro can be thought of as an inline version of `dsfn`.
The syntax mimics that of `case` in that the first argument is a value to evaluate one,
and the remaining arguments come in implicit pairs of *pattern*/*consequent*.
The first *consequent* is evaluated for which the *pattern* matches the given expression.

```clojure
(dscase '(1 2 (3 4))
  [[^Boolean a b] c d]
  12

  [^Boolean a [b c] ^String d]
  13

  [^Boolean a b [^String c ^String d]]
  (do
    (some-side-effect)
    (another-side-effect)
    14)

  [[^Number  a b [c d]] {}]
  15
)
```

The following is equivalent but arguably more readable as the type constraints and
structure constraints are presented separately.

```clojure
(dscase '(1 2 (3 4))
  ^{a Boolean}
  [[a b] c d] 
  12

  ^{a Boolean    d String}
  [a [b c] d]
  13

  ^{a Boolean
    [c d] String}
  [a b [c d]]
  (do
    (some-side-effect)
    (another-side-effect)
    14)

  ^{a Number} [a b [c d]]
  15
)
```

## Several special cases

- For both `dsfn` and also `dscase` the type constraint
`^{c String} [a b & c]` means that `c` is a sequence of elements each of type `String`
not that `c` has type string.

- If there are multiple type constraints on the same variable, the semantics is intersection.
I.e., both constraints are required.
`^{a (not (= 0))} [^Number a b]` this means that `a` is both a `Number` and also different from zero, effectively `a` has type `(and Number (not (= 0)))`.

- Multiple type constraints may accidentally make code unreachable.  
E.g., `^{a String} [^Number a b]` means that `a` is both a `Number` and also a `String`.  Equivalently, `(and Number String)` is the empty type. 
There is no such object so this pattern will never
match and the corresponding consequent code will be unreachable.

- A function such as `(fn [a & as] ...)` cannot be called on an empty
 argument list as an exception will be thrown.  However, using `let`,
`nil` can be destructured into `[a & as]` with both `a` and `as`
 bound to `nil`.  The `dscase` and `dsfn`
macros favor function application to let binding to determine
semantics.  For example, the following evaluates to `13`, not to
`12`.

```clojure
(dscase '()
  [a & as] 
  12

  []
  13
)
```

<!--  LocalWords:  memoized rte Dfa RTE DFA API Bdds Clojure ary
 -->
<!--  LocalWords:  destructured
 -->
