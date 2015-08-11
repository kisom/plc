# A propositional logic checker

## Introduction and Motivation

While reading The Little Prover and working through Write Yourself a
Scheme in 48 Hours, it seemed like it would be an interesting 
challenge to write a prover for simple propositional logic. This is
that attempt. It was written mostly late at night and is little more
than a toy.

## Current Status
theorem: (A | B) -> (A | (!A))
```
$ cabal run
Preprocessing executable 'plc' for plc-0.1.0.0...
Running plc...
PLC> axiom: A := #T
Evaluating theorem: Axiom: A ← True
Axiom A <- True entered.
Truth value: True
Result:
-------
Bindings:
A <- True
Result: True
Evaluating theorem: Axiom: A ← True
Axiom A <- True entered.
Truth value: True
PLC> axiom: B := #F
Evaluating theorem: Axiom: B ← False
Axiom B <- False entered.
Truth value: True
Result:
-------
Bindings:
A <- True
B <- False
Result: True
Evaluating theorem: Axiom: B ← False
Axiom B <- False entered.
Truth value: True
PLC> theorem: B | A
Evaluating theorem: Theorem: B | A
Evaluating term: B | A
Evaluating term: B
Evaluating term: f
Evaluating term: A
Evaluating term: t
Truth value: True
Result:
-------
Bindings:
A <- True
B <- False
Result: True
Evaluating theorem: Theorem: B | A
Evaluating term: B | A
Evaluating term: B
Evaluating term: f
Evaluating term: A
Evaluating term: t
Truth value: True
PLC> theorem: (A | B) -> (A | (!A))
Evaluating theorem: Theorem: A | B -> A | !A
Evaluating term: A | B -> A | !A
Evaluating term: A | B
Evaluating term: A
Evaluating term: t
Evaluating term: B
Evaluating term: f
Evaluating term: A | !A
Evaluating term: A
Evaluating term: t
Evaluating term: !A
Evaluating term: A
Evaluating term: t
Truth value: True
Result:
-------
Bindings:
A <- True
B <- False
Result: True
Evaluating theorem: Theorem: A | B -> A | !A
Evaluating term: A | B -> A | !A
Evaluating term: A | B
Evaluating term: A
Evaluating term: t
Evaluating term: B
Evaluating term: f
Evaluating term: A | !A
Evaluating term: A
Evaluating term: t
Evaluating term: !A
Evaluating term: A
Evaluating term: t
Truth value: True
PLC> quit
Goodbye.
```

## Syntax

Variable names must start with a letter, but they can contain any number
of digits or letters following this.

Terms must be separated by parens; unfortunately, this includes '!' for now.

The following are the operators:

+ !: negation
+ &: conjunction
+ |: disjunction
+ ~: equivalence
+ ->: implies

An axiom (or variable definition) is entered using "axiom: " followed
by the definition. A theorem (which will be checked) is entered with
"theorem: " followed by the definition. PLC is currently rather verbose,
and will display all steps in the evaluation, the current bindings, and
the truth value of the last axiom or theorem.

Exit the "prover" with "quit". Clear bindings with "clear". Show bindings
using "bindings".

## Source files

The implementation is covered in:

+ `Data.Logic.Propositional.Class` contains the type definitions for
  propositional logic theorems.

+ `Data.Logic.Propositional.Parser` will eventually contain the parser for reading
  theorems from a string.

+ `Data.Logic.Propositional` is the module for accessing the proof system.

## TODO

+ Improve parsing. It's fairly wonky.


