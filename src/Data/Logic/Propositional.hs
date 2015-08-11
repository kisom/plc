-- |
-- Module      : Data.Logic.Propositional
-- Copyright   : (c) K. Isom (2015)
-- License     : BSD-style
--
-- Maintainer  : coder@kyleisom.net
-- Stability   : stable
-- Portability : portable
-- 
-- This is a library for representing and implementing propositional logic
-- proofs. 

module Data.Logic.Propositional (
    Result (..)
  , prove
  , interactive
) where

import Control.Monad.Except
import Data.Logic.Propositional.Class
import qualified Data.Logic.Propositional.Parser as Parser
import qualified System.IO as IO

-------------------------------------------------------------------------------
-------------------------------- Truth Tables ---------------------------------
-------------------------------------------------------------------------------

truth :: Bindings -> Term -> ProofError Bool
truth bindings term = do
  liftIO $ putStrLn $ "Evaluating term: " ++ (show term)
  truthTable bindings term

-- | Evaluates a term in the context of a set of bindings; it returns a
--   'ProofError' containing either an error in the proof or a 'Bool'
--   result of the logical operation.
truthTable :: Bindings -> Term -> ProofError Bool
truthTable bindings (Value v)         = return v
truthTable bindings (Variable name)   = getName bindings name >>= truth bindings
truthTable bindings (Implies p q)     = do
    p' <- truth bindings p
    q' <- truth bindings q
    return (if p' && (not q')
         then False
         else True)
truthTable bindings (Conjunction p q) = do
    p' <- truth bindings p
    q' <- truth bindings q
    return $ p' && q'
truthTable bindings (Disjunction p q) = do
    p' <- truth bindings p
    q' <- truth bindings q
    return $ p' || q'
truthTable bindings (Equivalence p q) = do
    p' <- truth bindings p
    q' <- truth bindings q
    return $ p' == q'
truthTable bindings (Negation p)      = do
    p' <- truth bindings p
    return $ not p'

-- | A 'Result' contains the result of a proof.
data Result = Result Bindings [Theorem] Bool

showResult :: Result -> String
showResult (Result b _ v) = "Result:\n-------\nBindings:\n" ++ (show b)
                            ++ "Result: " ++ (show v)

instance Show Result where show = showResult

proofResult :: Proof -> Result
proofResult (Proof b thms) = Result b thms False

data Step = Step Bindings Bool

instance Show Step where
    show (Step b v) = "Result: " ++ show v ++ "\nBindings:\n" ++ show b

evaluate :: Bindings -> Theorem -> ProofError Step
evaluate bindings (Axiom name value) = do
    result <- bindName bindings name value
    liftIO $ putStrLn $ "Axiom " ++ name ++ " <- " ++ (show value) ++ " entered."
    return $ Step result True
evaluate bindings (Theorem term)     = truth bindings term >>=
                                       return . Step bindings

step :: Result -> ProofError Result
step (Result bindings (theorem:theorems) _) = do
  liftIO $ putStrLn $ "Evaluating theorem: " ++ (show theorem)
  result <- liftIO $ runExceptT $ evaluate bindings theorem
  case result of
    Right (Step bindings' v) -> do
      liftIO $ putStrLn $ "Truth value: " ++ (show v)
      return $ Result bindings' theorems v
    Left err                 -> throwError err
step (Result b thms v) = return $ Result b thms v

check :: Proof -> ProofError Result
check (Proof b thms) = check' (Result b thms False)
  where check' proof@(Result _ (_:_) _) = step proof >>= check'
        check' result@(Result _ [] _)    = return result

-- | 'prove' runs through the proof, ensuring that it is valid and that
--   it isn't inconsistent.
prove :: Proof -> ProofError Bindings
prove proof = do
  result <- liftIO $ runExceptT $ check proof
  case result of
    Left err                 -> throwError err
    Right (Result b _ True)  -> return b
    Right (Result _ _ False) -> throwError Invalid

prnFlush :: String -> IO ()
prnFlush s = putStr s >> IO.hFlush IO.stdout

readPrompt :: String -> IO String
readPrompt prompt = prnFlush prompt >> IO.getLine

displayResult :: Either Error Step -> IO String
displayResult proof = do
  return $ case proof of
                        Left err -> show err
			Right s  -> show s

evalLine :: Bindings -> String -> ProofError Step
evalLine b line = Parser.readExpr line >>= evaluate b

-- | interactive runs a sort of REPL; users enter their proof, one line
--   at a time, and PLC builds a proof from that. The REPL runs until
--   the user enters "quit".
interactive :: Bindings -> IO ()
interactive initial = do
  line <- readPrompt "PLC> "
  case line of
    "quit" -> putStrLn "Goodbye."
    "clear" -> putStrLn "Cleared bindings." >> interactive newBindings
    "bindings" -> putStrLn $ show initial
    _    -> interactiveEval initial line

interactiveEval :: Bindings -> String -> IO ()
interactiveEval b s = do
  let result = runExceptT $ evalLine b s
  result >>= displayResult >>= putStrLn
  result' <- result
  case result' of
    Left err -> interactive b
    Right (Step b' _) -> interactive b'

