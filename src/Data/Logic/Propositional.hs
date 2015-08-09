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

module Data.Logic.Propositional where

import Control.Monad.Except
import Data.Logic.Propositional.Class

-------------------------------------------------------------------------------
-------------------------------- Truth Tables ---------------------------------
-------------------------------------------------------------------------------

truth :: Bindings -> Term -> ProofError Bool
truth bindings term = do
  liftIO $ putStrLn $ "Evaluating term: " ++ (show term)
  truthTable bindings term

-- | 'truthTable' records the expected value of a given term.
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

data Result = Result Bindings [Theorem] Bool

showResult :: Result -> String
showResult (Result b _ v) = "Result:\n-------\nBindings:\n" ++ (show b)
                            ++ "Result: " ++ (show v)

instance Show Result where show = showResult

data Step = Step Bindings Bool

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

prove :: Proof -> ProofError Bindings
prove proof = do
  result <- liftIO $ runExceptT $ check proof
  case result of
    Left err                 -> throwError err
    Right (Result b _ True)  -> return b
    Right (Result _ _ False) -> throwError Invalid
