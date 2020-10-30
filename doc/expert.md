# Expert details w.r.t. RTE

## Confusion with negative semantics

As mentioned above the semantics of `:not` can be confusing and unintuitive.

The pattern `(:and :sigma (:not String))` 
matches a singleton sequence whose element is not a string; however,
`(:not String)` will match any sequence except a singleton sequence whose 
element is a string.  This becomes confusing in concatenations.

The pattern `(:cat (:not String) Long)` will refuse match 
`["hello" 4]` which is probably what the user intended.  But it will also match 
`[1 2 3 4 5]`.  Why?  Because `[1 2 3 4]` matches `(:not String)`, and `[5]`
matches `Long`.

If you really intended to exclude `["hello" 4]` and also exclude `[1 2 3 4 5]`,
the rte pattern needs more information.  For example, you might limit the 
sequence to sequences of two entries.  
`(:and (:cat :sigma :sigma) (:cat (:not String) Long))` which will refuse to 
match `[1 2 3 4 5]`, but will match `[1.2 3]`, because `[1.2]` is not a 
singleton sequence of String.

If you want to match a sequence like `[:x 100 :y 200 :z 300]` but not
if any of the values after the keyword is a String, you may use the following.
`(rte-match '(:* (:cat Keyword (:and :sigma (:not String)))) ...)`,
because `(:and :sigma (:not String))` will match any singleton
sequence whose element is NOT a string.

A feature which is not yet implemented will alleviate some of this confusion.
The *type designator* `(not ...)` will represent the set of all values except
those of a designated type.  `(:cat (not String) Long)` (once supported by RTE) 
will match sequence of length 2 whose second element is a `Long`, and whose 
first element is a member of the type `(not String)`, i.e., the set of all 
values which are not strings.

See section [Not yet implemented](../README.md#not-yet-implemented) for more
details of the proposed type designator syntax.


## Hierarchical Sequences

The `rte` type designator may be used to indicate hierarchical
sequences, i.e., sequences some of whose elements are themselves
sequences.  The type designator `(rte (:* integer?))` indicates the
set of sequences whose elements are sequences of any length consisting
of integers.

The rte pattern `(:* (rte (:+ integer?)))` matches a sequence of 0 or
more non-empty sequences whose elements are integers.

The pattern `(:* (:or (rte (:* String)) (rte (:* integer?))))` matches
a sequence of sequences each of which contains only strings or only
Integers, e.g.,

```clojure
(rte-match '(:* (:or (rte (:* String)) (rte (:* integer?))))
  [[1 2 3]
   ["hello" "world"]
   [10 20 30 40 50]])
==> true
```
but not
```clojure
(rte-match '(:* (:or (rte (:* String)) (rte (:* integer?))))
  [[1 2 3]
   ["hello" "world"]
   [10 "20" 30 "40" 50]])
==> false
```

## Abbreviating patterns

If you have several patterns which you want to name either for reuse
or readability, use the macro `with-rte`.

```clojure
(with-rte [::a (:permute Long Long String) ;; not quoted
           ::b (:permute Double Double String) ;; not quoted
           ]
  (let [rte (rte-compile '(:cat ::a ::a ::b ::b))]
    (rte-match rte [2 2 "hello"
                      2 "hello" 2
                      4.0 4.0 "world"
                      "world" 4.1 4.2]) ;; true
  ))
```

The semantics
of `(with-rte [...] ...)` vs `(rte ...)` may be confusing.
`(rte pattern)` designates a subtype of sequence whose
content matches the designated pattern.  On the other hand
`(with-rte [...] ...)` specifies sub-patterns which are interpolated into 
another pattern.

`(:cat (rte x) (rte y))` matches a two element sequence whose elements
are sequences *a* and *b*, where *a* matches the pattern *x* and *b*
matches the pattern *y*.

`(with-rte [::x ... ::y ...] (rte-match '(:cat ::x ::y) ...))` matches a 
sequence of two concatenated
sequence *a* and *b*, where *a* matches the pattern *::x* and *b*
matches the pattern *::y*.  E.g., 

```clojure
(with-rte [::x (:+ Long)
           ::y (:+ Double)]

  (let [pat (rte-compile '(:cat ::x  ::y))]
    ;; the same as (rte-compile '(:cat (:+ Long) (:+ Double)))
    (rte-match pat [1 2 3 1.2 3.4 5.6 7.8]) ;; true
    (rte-match pat [[1 2 3] [1.2 3.4 5.6 7.8]]) ;; false
  ))

(let [pat (rte-compile '(:cat (rte (:+ Long)) (rte (:+ Double))))]
  (rte-match pat [1 2 3 1.2 3.4 5.6 7.8]) ;; false
  (rte-match pat [[1 2 3] [1.2 3.4 5.6 7.8]]) ;; true
)
```

If you want to call `with-rte` programmatically, there is a lower
level function `call-with-rte` which may be easier to use for that
purpose.


## Algebra of RTEs

Given two RTEs it is possible to ask questions of habitation and
vacuity.  For example, given two patterns, one might ask, is there a
sequence which matches neither of the patterns?  Or to ask whether
every sequence which matches one also matches the other.

* `rte-trace` takes an rte pattern (compiled or not) and
  returns a sequence of type designators indicating the values of a
  sequence which will satisfy the original pattern.

```clojure
(rte-trace '(:and (:cat :sigma :sigma)
                  (:* integer?)))

==> [Byte Byte]

(rte-trace (rte-compile '(:and (:cat :sigma :sigma)
                               (:* (:and :sigma (:not Byte)))
                               (:* integer?))))

==> [Long Long]
```

* If you have two patterns, and you'd like to know what sequence matches one 
but not the other. For example to find a sequence which contains 1 or more 
`integer?` but does not contain 0 or more `Number`.

```clojure
(let [pattern1 '(:+ integer?)
      pattern2 '(:* Number)]
  (rte-trace (rte-compile `(:and ~pattern1 (:not ~pattern2)))))

==> [Byte]
```

Another example.  `(:* (:cat keyword? :sigma))` matches a sequence of
alternating `Keyword` anything pairs such as 
`[:x 100 :y "hello"]`. `(:cat (:* :sigma) keyword? (:* :sigma))` matches any
sequence which contains at least one `Keyword` somewhere.  So does
there exist a sequence which matches the first but not the second?

```clojure
(let [pattern1 '(:* (:cat keyword? :sigma))
      pattern2 '(:cat (:* :sigma) keyword? (:* :sigma))]
  (rte-trace (rte-compile `(:and ~pattern1 (:not ~pattern2)))))

==> []
```

We see that indeed the empty sequence matches `pattern1` but does not match 
`pattern2`.
