# `destructuring-fn` and `destructuring-case`

## destructuring-fn

The `destructuring-fn` macro can be thought of as a generalization of the clojure build-in `fn` macro.
However `destructuring-fn` offers two features that `fn` does not:

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
But, `((destructuring-fn [[a [b c] d] {}] 12) 1 2 3)`, simply returns `nil` as the data `(1 2 3)` 
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
((destructuring-fn ;; TODO doesn't work
   ([[[a b] c d] {}]  12)
   ([[a [b c] d] {}]  13)
   ([[a b [c d]] {}]  14))
 1 2 [3 4])
```

### Runtime selection by type

Use `destructuring-fn` to perform run-time format and type checking.
in addition to a structured argument list such as `[a [b c] d]`,
you may also specify type constraints, with as meta-data within
the argument list itself, or in the map specified after the
argument list.  Within the argument list you may only specify
very simply type constraints, i.e., symbols which designate types, such as `^Boolean` and `^Number`.
However in the map, you may specify Boolean cominations of any type designator
supported by `clojure-rte.type`.  See [Extensible types](genus.md) for more details.

```clojure
;; match if a is of type Number
((destructuring-fn [[^Number a [b c] d] {}] 
  12)
 1 [2 3] 4)
```

```clojure
;; match if a is of type Number and c is String
((destructuring-fn [[^Boolean a [b ^String c] d] {}] 
  12)
 true [2 "three"] 4)
```

The followign expression returns `nil` because although `(1 2 (3 4))`
matches the structure `[a b [c d]]`, the type of `a` is not `Boolean`.

```clojure
((destructuring-fn 
   ([[[^Boolean a b] c d] {}]  12)
   ([[^Boolean a [b c] d] {}] 13)
   ([[^Boolean a b [c d]] {}] 14))
 1 2 [3 4])
```

The following expression evaluates to 15.  Why? Because `(1 2 (3 4))` matches
`[^Number a b [c d]]` in type and structure.

```clojure
((destructuring-fn 
   ([[[^Boolean a b] c d] {}]  12)
   ([[^Boolean a [b c] d] {}] 13)
   ([[^Boolean a b [c d]] {}] 14)
   ([[^Number  a b [c d]] {}] 15))
 1 2 [3 4])
```

The map, `{}` in the above examples, specifies a map from variable name to type constraint.
The following two clauses are equivalent `[[^Boolean a [b ^String c] d] {}]` 
and `[[a [b c] d] {a Boolean c String}]`.  With simple type specifiers, the two forms are equivalent.
However, the second form allows more flexible type constraints such as the following.

- `[[a [b c] d] {[a b] Boolean c String}]` --- `a` and `b` are both `Boolean`, and `c` is String.
- `[[a [b c] d] {a  Boolean b (or Boolean String) String}]` --- `a` is `Boolean`, `c` is String, and `b` is either `Boolean` or `String`.
- `[[a [b c] d] {a Boolean b (and Number (not Long))}]` --- `a` is `Boolean`, and `b` is either `Number` but not `Long`.
- `[[a b c] {a (member -1 0 1) b (and (Number (not (= 0))))}]` --- `a` is -1, 0 or 1, and `b` is a `Number` different from 0.



## destructuring-case

The `destructuring-case` macro can be thought of as an inline version of `destructuring-fn`.
The syntax mimics that of `case` in that the first argument is a value to evaluate one,
and the remaining arguments come in implicit pairs of *pattern*/*consequent*.
The first *consequent* is evaluated for which the *pattern* matches the given expression.

```clojure
(destructuring-case '(1 2 (3 4))
  [[[^Boolean a b] c d] {}] 
  12

  [[^Boolean a [b c] ^String d] {}]
  13

  [[^Boolean a b [^String c ^String d]] {}]
  (do
    (some-side-effect)
    (another-side-effect)
    14)

  [[^Number  a b [c d]] {}]
  15
)
```

The following is equivent but arguably more readable as the type constraints and
structure constraints are presented separately.

```clojure
(destructuring-case '(1 2 (3 4))
  [[[a b] c d] 
   {a Boolean}] 
  12

  [[a [b c] d]
   {a Boolean
    d String}]
  13

  [[a b [c d]]
   {a Boolean
    [c d] String}]
  (do
    (some-side-effect)
    (another-side-effect)
    14)

  [[a b [c d]] {a Number}]
  15
)
```

## Several special cases

- For both `destructuring-fn` and also `destructuring-case` the type constraint
`[[a b & c] {c String}` means that `c` is a sequence of elements each of type `String`
not that `c` has type string.

- If there are multiple type constratins on the same variable, the semantics is intersection.
I.e., both constraints are required.
`[[^Number a b] {a (not (= 0))}]` this means that `a` is both a `Number` and also different from zero, effectively `a` has type `(and Number (not (= 0)))`.

- Multiple type constraints may accidentally make code unreachable.  
E.g., `[[^Number a b] {a String}]` means that `a` is both a `Number` and also a `String`.  Equivalently, `(and Number String)` is the empty type. 
There is no such object so this pattern will never
match and the corresponding consequent code will be unreachable.

- A function such as `(fn [a & as] ...)` cannot be called on an empty
 argument list as an exception will be thrown.  However, using `let`,
`nil` can be destructured into `[a & as]` with both `a` and `as`
 bound to `nil`.  The `destructuring-case` and `destructuring-fn`
macros favor function application to let binding to determine
semantics.  For example, the following evaluates to `13`, not to
`12`.

```clojure
(destructuring-case '()
  [[a & as]    {}] 
  12

  [[]       {}]
  13
)
```
