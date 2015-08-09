# A propositional logic checker

## Introduction and Motivation

While reading The Little Prover and working through Write Yourself a
Scheme in 48 Hours, it seemed like it would be an interesting intellectual
challenge to write a prover for simple propositional logic. This is
that attempt.

## Current Status

```
*Main> :t testProof1
testProof1 :: Proof
*Main> :info Proof
data Proof = Proof Bindings [Theorem]
        -- Defined at src/Data/Logic/Propositional/Class.hs:135:1
instance Show Proof
  -- Defined at src/Data/Logic/Propositional/Class.hs:150:10
*Main> testProofs
[Proof:
Axiom: A ← True
Theorem: A ^ !A
,Proof:
Axiom: A ← True
Theorem: A ~ A
,Proof:
Axiom: A ← True
Theorem: t ~ t
,Proof:
Theorem: A ^ !A
,Proof:
Axiom: A ← True
Axiom: B ← False
Theorem: A ~ B
]
*Main> runProofs testProofs

Checking proof
Proof:
Axiom: A ← True
Theorem: A ^ !A

Evaluating theorem: Axiom: A ← True
Evaluating theorem: Theorem: A ^ !A
Evaluating term: A ^ !A
Evaluating term: A
Evaluating term: t
Evaluating term: !A
Evaluating term: A
Evaluating term: t
Proof is consistent
Null proof; bindings:
A <- True


Checking proof
Proof:
Axiom: A ← True
Theorem: A ~ A

Evaluating theorem: Axiom: A ← True
Evaluating theorem: Theorem: A ~ A
Evaluating term: A ~ A
Evaluating term: A
Evaluating term: t
Evaluating term: A
Evaluating term: t
Proof is consistent
Null proof; bindings:
A <- True


Checking proof
Proof:
Axiom: A ← True
Theorem: t ~ t

Evaluating theorem: Axiom: A ← True
Evaluating theorem: Theorem: t ~ t
Evaluating term: t ~ t
Evaluating term: t
Evaluating term: t
Proof is consistent
Null proof; bindings:
A <- True


Checking proof
Proof:
Theorem: A ^ !A

Evaluating theorem: Theorem: A ^ !A
Evaluating term: A ^ !A
Evaluating term: A
Proof is inconsistent: The variable A is unbound.

Checking proof
Proof:
Axiom: A ← True
Axiom: B ← False
Theorem: A ~ B

Evaluating theorem: Axiom: A ← True
Evaluating theorem: Axiom: B ← False
Evaluating theorem: Theorem: A ~ B
Evaluating term: A ~ B
Evaluating term: A
Evaluating term: t
Evaluating term: B
Evaluating term: f
Proof is inconsistent: The term A ~ B is inconsistent.
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


