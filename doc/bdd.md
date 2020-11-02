# clojure-rte.bdd

# Binary Decision Diagrams

This package implements a type algebra based on Binary Decision Diagrams (BDDs)
in the [Clojure](https://clojure.org) programming language.
The theory of how BDDs are used to reason about Common Lisp types can be found here:
[Representing and Computing with Types in Dynamically Typed Languages](https://www.lrde.epita.fr/wiki/Publications/newton.18.phd).


A Bdd is implemented as a record
```clojure
(defrecord Bdd
  [label positive negative])
```
Thus a Bdd may be allocated as `(bdd. ...)`.
However, applications are not intended to allocate a bdd using the record interface
directly. Instead an application should allocate Bdds using `(bdd ...)` or one
of the Boolean operators `(bdd-and ...)`, `(bdd-or ...)`, `(bdd-and-no ...)`, or `(bdd-not ...)`.

## API

### (`bdd` type-designator)
Allocate a Bdd using a [type designator](genus.md).

```clojure
(with-bdd-hash []
  (bdd '(and Number (not Long)))
```

### (`gen-random` [max-depth])
Generate a random Bdd using the type-designators determined by `sample-types`.

### (`itenf` bdd)
Serialize a Bdd into if-then-else-normal-form.
```clojure
(with-bdd-hash []
  (itenf (bdd '(or (= 1)
                   (and Number (not Long))))))
;; returns -->
(or
 (and Number (or (and Long (= 1)) (not Long)))
 (and (not Number) (= 1)))
```

### (`dnf` bdd)
Serialize a Bdd into disjunctive-normal-form, i.e. an OR of ANDs
```clojure
(with-bdd-hash []
  (dnf (bdd '(and (or String Long)
                  (or Number 
                      (not (member "a" "b"))
                      (not (= 1)))))))
;; returns -->
(or Long (and String (not (member "a" "b"))) (member "a" "b"))
```

### (`with-bdd-hash` [] & body...)

This macro wraps a piece of code which needs to allocate Bdds. The macro
wraps a call to the function call-with-bdd-hash, which provides an environment,
of sorts, which makes it possible to allocate and manipulate Bdd instances.
If `with-bdd-hash` is called recursively (intentionally or accidentally), the
inner-most call recognizes this and does not re-bind any dynamic variables,
thus the inner-most call is innocuous and harmless.


### (`bdd-and` & bdds)
Perform a Boolean AND on 0 or more Bdds.

### (`bdd-or` & bdds)
Perform a Boolean OR on 0 or more Bdds.

### (`bdd-and-not` & bdds)
Perform a relative complement operation on two (or more) Bdds.
This is not implemented for the 0-ary nor 1-ary case.

### (`bdd-not` bdd)
Perform a Boolean not of a given Bdd

### (`bdd-typep` value bdd)
Given a value in question, and a Bdd representing a type designator,
determine whether the value is an element of the designated type.

```clojure
(with-bdd-hash []
  (let [bdd (bdd '(and Number (not Long)))]
    (bdd-typep 12.0 bdd)))
;; returns true

clojure-rte.bdd> (with-bdd-hash []
  (let [bdd (bdd '(and Number (not Long)))]
    (bdd-typep 2 bdd)))
;; returns false
```

### (`bdd-disjoint?` bdd1 bdd2)
Given two Bdds, determine whether it can be proven that the intersection of the
types they represent is empty.
If it cannot be proven that they are disjoint, `false` is returned.

### (`bdd-type-disjoint?` type-1 type-2)
Given two type designators, use Bdds to determine whether they are disjoint.
If it cannot be proven that they are disjoint, `false` is returned.

### (`bdd-type-subtype?` type-sub type-super)
Given two type designators, use Bdds to determine whether one is a subtype of the other.
If it cannot be proven, `false` is returned.

<!--  LocalWords:  memoized rte Dfa RTE DFA API Bdds Clojure ary
 -->
