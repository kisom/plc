module Main where

import Control.Monad.Except
import Data.IORef
import Data.Logic.Propositional 
import Data.Logic.Propositional.Class
import Data.String

testThm = Theorem (Conjunction (Variable "A") (Negation (Variable "A")))
testProof = (nullBindings, [Axiom "A" True, testThm])
testProof' = (nullBindings, [Axiom "A" True, (Theorem (Equivalence (Variable "A") (Variable "A")))])

showProofBindings :: Proof -> IO ()
showProofBindings (bindings, _) = do
    bindings' <- liftIO bindings
    showBindings bindings'

displayProofBindings :: ProofError Proof -> IO ()
displayProofBindings proof = do
    proof' <- runExceptT proof
    case proof' of
        Left  err -> putStrLn (show err)
        Right val -> showProofBindings val

checkProof proof = do
    result <- runExceptT $ check proof
    case result of
        Left err -> return $ show err
        Right v  -> return "OK"

main = (putStrLn "Test theorem:")
        >> (putStrLn $ show testThm)
        >> putStrLn "Evaluation:"
        >> (checkProof testProof >>= putStrLn)
