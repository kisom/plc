module Main where

import Control.Monad.Except
import Data.IORef
import Data.Logic.Propositional 
import Data.Logic.Propositional.Class
import Data.String

testThm = Theorem (Conjunction (Variable "A") (Negation (Variable "A")))
testProof1 = Proof newBindings [Axiom "A" True, testThm]
testProof2 = Proof newBindings
             [Axiom "A" True, (Theorem (Equivalence (Variable "A") (Variable "A")))]
testProof3 = Proof newBindings
             [Axiom "A" True, (Theorem (Equivalence (Value True) (Value True)))]
testProof4 = Proof newBindings [testThm]
testProof5 = Proof newBindings
             [Axiom "A" True, Axiom "B" False,
              Theorem
              (Equivalence (Variable "A") (Variable "B"))]

testProofs = [testProof1, testProof2, testProof3, testProof4, testProof5]

checkProof :: Proof -> IO String
checkProof proof = do
  result <- runExceptT $ check proof
  case result of
    Left err  -> return $ "Proof is inconsistent: " ++ (show err)
    Right p   -> return $ "Proof is consistent\n" ++ (show p)

runProof :: Proof -> IO ()
runProof proof = do
  putStrLn "\nChecking proof"
  putStrLn $ show proof
  checkProof proof >>= putStrLn

runProofs :: [Proof] -> IO ()
runProofs (p:p') = runProof p >> runProofs p'
runProofs []     = return ()

main = runProofs testProofs
