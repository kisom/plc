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

-- | 'truthTable' records the expected value of a given term.
truthTable :: Bindings -> Term -> ProofError Bool
truthTable bindings (Value v)         = return v
truthTable bindings (Variable name)   = getName bindings name
truthTable bindings (Implies p q)     = do
    p' <- truthTable bindings p
    q' <- truthTable bindings q
    return (if p' && (not q')
         then False
         else True)
truthTable bindings (Conjunction p q) = do
    p' <- truthTable bindings p
    q' <- truthTable bindings q
    return $ p' && q'
truthTable bindings (Disjunction p q) = do
    p' <- truthTable bindings p
    q' <- truthTable bindings q
    return $ p' || q'
truthTable bindings (Equivalence p q) = do
    p' <- truthTable bindings p
    q' <- truthTable bindings q
    return $ p' == q'
truthTable bindings (Negation p)      = do
    p' <- truthTable bindings p
    return $ not p'

evaluate :: Bindings -> Theorem -> ProofError Bool
evaluate bindings (Axiom name value) = bindName bindings name value
evaluate bindings (Theorem term)     = truthTable bindings term

step :: Proof -> ProofError Proof
step (bindings, (theorem:theorems)) = do
    bindings' <- liftIO bindings
    liftIO $ showBindings bindings'
    result    <- liftIO $ runExceptT $ evaluate bindings' theorem
    case result of
        Left   err -> throwError err
        Right val  -> return (bindings, theorems)

check :: Proof -> ProofError Bool
check (_, []) = return True
check proof = step proof >>= check

