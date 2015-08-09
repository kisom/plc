# A propositional logic checker

## Introduction and Motivation

While reading The Little Prover and working through Write Yourself a
Scheme in 48 Hours, it seemed like it would be an interesting intellectual
challenge to write a prover for simple propositional logic. This is
that attempt.

## Current Status

```
*Data.Logic.Propositional> :load src/Main.hs 
[1 of 3] Compiling Data.Logic.Propositional.Class ( src/Data/Logic/Propositional/Class.hs, interpreted )
[2 of 3] Compiling Data.Logic.Propositional ( src/Data/Logic/Propositional.hs, interpreted )
[3 of 3] Compiling Main             ( src/Main.hs, interpreted )
Ok, modules loaded: Data.Logic.Propositional, Data.Logic.Propositional.Class, Main.
*Main> :info testProof7
testProof7 :: Proof     -- Defined at src/Main.hs:45:1
*Main> :info Proof
data Proof = Proof Bindings [Theorem]
        -- Defined at src/Data/Logic/Propositional/Class.hs:140:1
instance Show Proof
  -- Defined at src/Data/Logic/Propositional/Class.hs:155:10
*Main> runProofs [testProof3, testProof7]

Checking proof
Proof:
Axiom: A ← True
Theorem: A ^ !A

Evaluating theorem: Axiom: A ← True
Axiom A <- True entered.
Truth value: True
Evaluating theorem: Theorem: A ^ !A
Evaluating term: A ^ !A
Evaluating term: A
Evaluating term: t
Evaluating term: !A
Evaluating term: A
Evaluating term: t
Truth value: False
Proof is invalid: The proof evaluated to False.

Checking proof
Proof:
Axiom: A ← True
Axiom: B ← True
Theorem: A v !A => A ~ !B

Evaluating theorem: Axiom: A ← True
Axiom A <- True entered.
Truth value: True
Evaluating theorem: Axiom: B ← True
Axiom B <- True entered.
Truth value: True
Evaluating theorem: Theorem: A v !A => A ~ !B
Evaluating term: A v !A => A ~ !B
Evaluating term: A v !A
Evaluating term: A
Evaluating term: t
Evaluating term: !A
Evaluating term: A
Evaluating term: t
Evaluating term: A ~ !B
Evaluating term: A
Evaluating term: t
Evaluating term: !B
Evaluating term: B
Evaluating term: t
Truth value: False
Proof is invalid: The proof evaluated to False.
*Main> :reload
[3 of 3] Compiling Main             ( src/Main.hs, interpreted )
Ok, modules loaded: Data.Logic.Propositional, Data.Logic.Propositional.Class, Main.
*Main> runProof testProof7

Checking proof
Proof:
Axiom: A ← True
Axiom: B ← True
Theorem: A v !A => A ~ B

Evaluating theorem: Axiom: A ← True
Axiom A <- True entered.
Truth value: True
Evaluating theorem: Axiom: B ← True
Axiom B <- True entered.
Truth value: True
Evaluating theorem: Theorem: A v !A => A ~ B
Evaluating term: A v !A => A ~ B
Evaluating term: A v !A
Evaluating term: A
Evaluating term: t
Evaluating term: !A
Evaluating term: A
Evaluating term: t
Evaluating term: A ~ B
Evaluating term: A
Evaluating term: t
Evaluating term: B
Evaluating term: t
Truth value: True
Proof is valid
A <- True
B <- True
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


