module Main where

import Control.Monad.Except
import Data.IORef
import Data.Logic.Propositional 
import Data.Logic.Propositional.Class
import Data.String

testThm = Theorem (Conjunction (Variable "A") (Negation (Variable "A")))

varA = Variable "A"
varB = Variable "B"


-- This tests the basic non-bindings proving. True *should* true, at
-- least in this universe.
testProof1 = Proof newBindings
             [(Theorem (Equivalence (Value True) (Value True)))]

-- If the prover can't prove that A == A, time to pack it up and go home.
testProof2 = Proof newBindings
             [Axiom "A" True, (Theorem (Equivalence varA varA))]

-- A basic sanity check: given an axiom (A <- True), will a basic
-- proof be shown as consistent?
testProof3 = Proof newBindings [Axiom "A" True, testThm]

-- An empty set of bindings and no axioms should be unprovable.
testProof4 = Proof newBindings [testThm]

-- Trying to prove true = false should fail.
testProof5 = Proof newBindings
             [Axiom "A" True, Axiom "B" False,
              Theorem
              (Equivalence (Variable "A") (Variable "B"))]

-- The following proof should demonstrate that ordering theorems matters.
testProof6 = Proof newBindings
             [Axiom "A" True,
              Theorem
              (Equivalence (Variable "A") (Variable "B")),
              Axiom "B" True]

-- The following proof is a little longer.
testProof7 = Proof newBindings
             [Axiom "A" True
             ,Axiom "B" True
             ,Theorem (Implies
              (Disjunction varA (Negation varA))
              (Equivalence varA varB))]

testProofs = [testProof1, testProof2, testProof3, testProof4, testProof5
             ,testProof6]

checkProof :: Proof -> IO String
checkProof proof = do
  result <- runExceptT $ prove proof
  case result of
    Left err  -> return $ "Proof is invalid: " ++ (show err)
    Right p   -> return $ "Proof is valid\n" ++ (show p)

runProof :: Proof -> IO ()
runProof proof = do
  putStrLn "\nChecking proof"
  putStrLn $ show proof
  checkProof proof >>= putStrLn

runProofs :: [Proof] -> IO ()
runProofs (p:p') = runProof p >> runProofs p'
runProofs []     = return ()

main = interactive newBindings
