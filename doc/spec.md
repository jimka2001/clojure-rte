<!--
 Copyright (c) 2020 EPITA Research and Development Laboratory

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

# Spec integration into Genus

Spec is incorporated into RTE by incorporating the `(spec ...)` type designator
into Genus.


As explained in [How to extend the type system](doc/genus.md#how-to-extend-the-type-system)
the Genus type system can be extended by applications.  To add a new type
an application must provide several methods which makes it possible for Genus
to deal with that type: e.g., 
`-inhabited?`, `-disjoint?`, `-subtype?`, `typep`, and `-canonicalize-type`, `valid-type?`.


## Methods defined in `genus-spec` which implement the `spec` type designator

The namespace `clojure-rte.genus-spec` contains the methods which implement the `spec`
type.

```clojure
(require '[clojure-rte.genus :as gns])

(defmethod gns/typep 'spec [a-value [_ pattern]]
  ;; returns true/false
  ...)

(defmethod gns/-inhabited? 'spec [[_ pattern]]
  ;; returns true/false/:dont-know
  ...)

(defmethod gns/valid-type? 'spec [[_ pattern]])
  ;; returns true/false
  ...)

(defmethod gns/-disjoint? 'spec [t1 t2]
  ;; returns true/false/:dont-know
  ...)

(defmethod gns/-subtype? 'spec [sub super]
  ;; returns true/false/:dont-know
  ...)

(defmethod gns/-canonicalize-type 'spec [[_spec pattern]]
  ;; returns same or simpler but equivalent type designator
  ...)
```

## Using `spec` type designators in applications


If your application requires genus-spec e.g.,
```clojure
(require '[clojure-rte.genus :as gns])
(require '[clojure-rte.genus-spec :as gs])
```

A type designator such as `(spec X)` designates the set of all Clojure
objects which validate the spec X; i.e., validates in the sense of the
`(clojure.spec.alpha/valid? X)`.  Here are some examples.

```clojure
(require '[clojure.spec.alpha :as s])
(require '[clojure-rte.genus :as gns])
(require '[clojure-rte.rte-construct :as rte])
(require '[backtick :refer [template]])

(s/def ::test-spec-1 (s/* (s/alt :1  (s/cat :3 neg? :4 even?)  
                                 :2  (s/cat :5 odd? :6 pos?))))
(s/def ::test-spec-2 (s/or :1 string?
                           :2 (s/and int? #(even? %))))

(gns/typep "hello" '(spec ::test-spec-2))
(gns/typep "hello" (template (spec ~(s/or :1 (s/and int? odd?)
                                          :2 string?))))

(rte/match '(rte (:* (:or String (spec ::test-spec-1))))
           ["hello"
            "world"
            [-1 2 -2 -2 3 3 -3 3]])
```


<!--  LocalWords:  RTE rte pos clojure
 -->
