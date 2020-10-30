# dfa.bdd

# Deterministic Finite Automata

The dfa library implements a data structure (defined by `defrecord`)
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
map representing the finite atomaton.

