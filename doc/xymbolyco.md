# Xymbolyco: Sigma Complete Deterministic Finite Automata

<img src="../img/example-dfa.png" alt="Example Finite Automaton" width="600"/>

The `clojure-rte.xymbolyco` library implements a data structure (defined by `defrecord`)
which models a deterministic finite automaton.

## Dfa --- `(defrecord Dfa [pattern states exit-map])`
* `:pattern` --- an rte pattern which this Dfa represents
* `:states` --- a map of integer state id to `State` (a data structure described below)
* `:exit-map` --- a map which maps state id to any value.  The purpose is to specify which value
should be returned by an engine which walks through the Dfa using an input sequence and
arrives in an accepting state.

## State --- `(index initial accepting pattern transitions])`
* `:index` --- the index of this state in the array
* `:accepting` --- Boolean true/false indicating whether this state is a  final/accepting state.
* `:initial` --- Boolean indicating whether this is an initial state
* `:pattern` --- the derivative value representing an rte pattern matching
any tail of the input sequence which is accepting from this point
onward.
* `:transitions` --- A list of pairs, each pair is a 2 element array of the form
`[type next-state]`, e.g., `[clojure.lang.Keyword 1]`
which means if the value at the head of the sequence is of type
`clojure.lang.Keyword`, then go to state 1.  The type is some type designator
as specified in the `genus` library.  The state index is some index of the state
map representing the finite automaton.

# Dfa API

## `make-dfa` 
Dfa factory function, which checks consistency.  
1-ary version, `(make-dfa attribute-map)`, takes an attribute map with the keys of the Dfa record.
2-ary version, `(make-dfa dfa attribute-map)`, creates a new Dfa by copying the given one but overriding
with any given attributes in the `attribute-map`.
## `rte-compile`
Creates a Dfa from an rte.  See [RTE API](api.md).
## `complete `
Render complete the given Dfa.
If it is already complete, then simply return it,
otherwise compute a new Dfa which has been completed, but
adding a sink state if necessary and add at most one transition
per state to the sink state.

## `minimize`
Accepts an object of type Dfa, and returns a new object of type Dfa
implementing the minimization of the state machine according to the
Hopcroft minimization algorithm.

## `trim`
Creates a new Dfa from the given Dfa containing only accessible and co-accessible
states.  Warning, this removes the sink state if there is one.  The result is
that the computed Dfa may not any longer be complete.
Don't remove the initial state.

## `complement`
Accepts an object of type Dfa, and returns a new object of type Dfa
whose recognition languages is the complement of the given Dfa.
This is done by replacing accepting states with non-accepting states
and non-accepting with accepting.  The transformation looses exit-value
information.

## `extract-rte`
This is an internal function, the public API is `clojure-rte.rte-core/dfa-to-rte`, See [dfa-to-rte](api.md/#dfa-to-rte-dfa).
Accepts an object of type Dfa, and returns a map which associates
exit values of the Dfa with canonicalized rte patterns of the accepting
language. If there are no accepting states in the Dfa, an empty map `{}`
is returned.

## `synchronized-product`
Assuming that the given Dfas are complete, we compute the synchronized cross product SXP
of the two Dfas.
*  `f-arbitrate-accepting` - binary function which accepts two Boolean values, `[a1,a2]`
Then function is called when determining whether a computed state in the SXP
should be an accepting state.  `a1` indicates whether the state from `dfa-1` is
accepting.  `a2` indicates whether the state from `dfa-2` is accepting.
To effectuate the intersection of `dfa-1` with `dfa-2`, `f-arbitrate-accepting` should
perform an `(and a1 a2)`.
* `f-arbitrate-exit-value` - binary function called with `[q1,q2]`.  `q1` is an accepting state
of `dfa-1`.  `q2` is an accepting state in `dfa-2`.
`f-arbitrate-exit-value` is called when `q1` and `q2` are both accepting state or
when neither is an accepting state.   In the case that only `q1` or only `q2`
is an accepting state, this accepting state's exit value is used in the SXP.
`f-arbitrate-exit-value` should return the exit value for the state in the SXP.

## `synchronized-union`
Compute the union of two Dfas.  I.e., compute a Dfa which
will recognize sequences which either `dfa-1` or `dfa-2` recognizes.
If some sequence is recognized both by `dfa-1` and `dfa-2`, then
the exit value is determined by `dfa-1`, and the exit-value of
`dfa-2` is silently ignored.

## `synchronized-intersection`
Compute the intersection of two Dfas. I.e., compute the Dfa which
will recognized any sequence which is recognized by `dfa-1` and also
by `dfa-2`.

## `synchronized-xor`
Compute the xor of two Dfas. I.e., compute the Dfa which
will recognized any sequence which is recognized by `dfa-1` or
by `dfa-2` but not by both.

## `dfa-equivalent`
Returns a Boolean indicating whether the two given Dfas
recognize the same language.

# Dfa as a graph

The function `clojure-rte.dot/dfa-to-dot` can be used to generate a graphical
image of a Dfa.  See [RTE API Debugging](api.md/#debugging)

<!--  LocalWords:  rte Dfa alt img png src xymbolyco RTE API Dfas 
 -->
<!--  LocalWords:  Hopcroft  SXP
 -->
