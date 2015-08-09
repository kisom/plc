# A propositional logic checker

## Introduction and Motivation

While reading The Little Prover and working through Write Yourself a
Scheme in 48 Hours, it seemed like it would be an interesting intellectual
challenge to write a prover for simple propositional logic. This is
that attempt.

## Current Status

```
*Main> checkProof testProof''
Evaluating Axiom: A ← True
Evaluating Theorem: t ~ t
"OK"
```

## Source files

The implementation is covered in:

+ `Data.Logic.Propositional.Class` contains the type definitions for
  propositional logic theorems.

+ `Data.Logic.Propositional.Parser` will eventually contain the parser for reading
  theorems from a string.

+ `Data.Logic.Propositional` is the module for accessing the proof system.

## TODO

+ Add parsing; right now, proofs are constructed via the type constructors,
  which is rather unwieldy. Something to think about: precedence and order
  of operations.

+ Bindings aren't working:

```
*Main> checkProof testProof
Evaluating Axiom: A ← True
Evaluating Theorem: A ^ !A
"The variable A is unbound."
*Main> checkProof testProof'
Evaluating Axiom: A ← True
Evaluating Theorem: A ~ A
"The variable A is unbound."
```

