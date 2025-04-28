# gigalog

Gigalog is a minimal parallel Datalog engine supporting fixpoint evaluation and stratified negation.

It provides an interpreter which supports wildcards, negation, strings and constants. It also supports input and output statements, comments, as well as recursion and mutual recursion.

Later on I plan to add support for integers and comparison operators to facilitate more useful programming.

Here are some sample programs.

```datalog
% samegeneration.dl
.input Parent("test.csv")
Person(X) :- Parent(X, _).
Person(X) :- Parent(_, X).
SameGeneration(X, X) :- Person(X).
SameGeneration(X, Y) :- Parent(X, P), SameGeneration(P, Q), Parent(Y, Q).
.output SameGeneration
```

```datalog
link(a, b).
link(b, c).
link(c, d).
link(d, e).

path(X, Y) :- link(X, Y).
path(X, Y) :- step(X, Y).

step(X, Y) :- link(X, Z), path(Z, Y).
```
