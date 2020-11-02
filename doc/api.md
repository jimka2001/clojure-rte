# RTE API

## (`rte-compile` rte-pattern)
Compiles an rte pattern into an internal representation representing a
DFA (deterministic finite automaton).  The representation is designed
to be human readable for debugging purposes, but its structure is not
intended as an API.  I.e., the structure might change in future
releases.

The return value of this function is memoized.  Thus if the same rte
pattern is encountered again the previously compiled Dfa is returned
as an `O(1)` operation.

The return value of `rte-compile` may be used as first argument of `rte-match`.

Note about performance.  You may force an rte to be compiled at
program load/compile time by defining at top level using `def`.

```clojure
(def my-rte (rte-compile ...)) ;; compile rte at load time
```

In the following case the RTE will be compiled at run-time, the first
time the code is evaluated.

```clojure
(defn foo [args ...]
  (let [rte (rte-compile '(:cat (:* Long) (:* Double) (:* String)))]
    ...
    ... (rte-match rte ...)
    ...
    ))
```


## (`rte-match` rte items)

Returns Boolean value.

Match a given sequence against a pre-compiled RTE pattern.

```clojure
(let [rte (rte-compile '(:cat (:* String) (:* Long) (:* Double)))]
  (rte-match rte ["hello" "world" 1 2 3]) ;; true
  (rte-match rte ["hello" "world" 1.0 2.0 3.0]) ;; true
  (rte-match rte ["hello" "world" 1.0 2.0 3]) ;; false
  )
==> true
```

```clojure
(rte-match '(:cat (:* String) (:* Double)) ["hello" "world" 1 2 3])
==> true
```



## (`with-rte` bindings & body)
See section [Abbreviating patterns](#abbreviating-patterns) for more information

## (`rte-trace` rte)

See section [Algebra of RTEs](#algebra-of-rtes) for more information.



## (`dfa-to-rte` dfa)
Extract rtes from a Dfa.  Since accepting states are distinguishable, a map is returned rather
than simply an rte.  The map associates each exit-value with an rte.

This does not guarantee to give the exact same 
syntactical form as you started with.
```clojure
(rte-to-dfa
   (dfa-to-rte (rte-to-dfa '(:* (:cat String Integer)))))
==> {true (:or (:cat String
                     (:* (:cat Integer String))
                     Integer)
               :epsilon)}

(rte-to-dfa
   (dfa-to-rte (rte-to-dfa '(:* (:cat String Integer)) 12)))
==> {12 (:or (:cat String
                   (:* (:cat Integer String))
                   Integer)
             :epsilon)}
```

The association of exit value to rte is important if the Dfa in question is
the result of a synchronized cross product, such as `synchronized-union` of two
Dfas each with a different exit value.


```clojure
(require '[clojure-rte.xymbolyco :as dfa])

(let [dfa-1 (rte-to-dfa '(:* (:cat String Long)) :string-long)
      dfa-2 (rte-to-dfa '(:* (:cat String Short)) :string-short)
      dfa (dfa/synchronized-union  dfa-1 dfa-2)]
  (dfa-to-rte dfa))
==> {:string-long  (:or (:cat String
                              Long
                              (:* (:cat String Long))) 
                        :epsilon),
     :string-short (:cat String 
                         Short 
                         (:* (:cat String Short)))
    }
```


# Debugging

Once a Dfa has been created with a call to `rte-compile` or `rte-to-dfa`, you 
may draw the corresponding graph using the `dfa-to-dot` function.

```clojure
(clojure-rte.dot/dfa-to-dot
  (rte-compile '(:and (:cat :sigma :sigma) (:cat (:not String) Long)))
  :title "Example"
  :abbrev false
  :state-legend false
  :view true)
```
<img src="../img/example-dfa.png" alt="Example Finite Automaton" width="600"/>



```clojure
(clojure-rte.dot/dfa-to-dot
  (rte-compile '(:and (:cat :sigma :sigma) (:cat (:not String) Long)))
  :title "Example"
  :abbrev true
  :state-legend false
  :view true)
```
<img src="../img/example-dfa-2.png" alt="Example Finite Automaton" width="400"/>

```clojure
(clojure-rte.dot/dfa-to-dot
  (clojure-rte.dot/minimize
    (rte-compile '(:and (:cat :sigma :sigma) (:cat (:not String) Long))))
  :title "Example"
  :abbrev true
  :state-legend false
  :view true)
```
<img src="../img/example-dfa-3.png" alt="Example Finite Automaton" width="400"/>



<!--  LocalWords:  memoized rte Dfa RTE DFA API
 -->
