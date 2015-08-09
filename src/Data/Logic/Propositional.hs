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
    if p' == q'
       then return True
       else throwError $ Inconsistent (Equivalence p q)
truthTable bindings (Negation p)      = do
    p' <- truth bindings p
    return $ not p'

data Step = Step Bindings Bool

evaluate :: Bindings -> Theorem -> ProofError Step
evaluate bindings (Axiom name value) = do
    result <- bindName bindings name value
    return $ Step result True
evaluate bindings (Theorem term)     = truth bindings term >>=
                                       return . Step bindings

step :: Proof -> ProofError Proof
step (Proof bindings (theorem:theorems)) = do
  liftIO $ putStrLn $ "Evaluating theorem: " ++ (show theorem)
  result <- liftIO $ runExceptT $ evaluate bindings theorem
  case result of
    Right (Step bindings' _) -> return $ Proof bindings' theorems
    Left err                 -> throwError err
step proof = return proof

check :: Proof -> ProofError Proof
check proof@(Proof _ (_:_)) = step proof >>= check
check proof@(Proof _ [])    = return $ proof

