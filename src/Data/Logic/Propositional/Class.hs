{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-- |
-- Module      : Data.Logic.Propositional.Class
-- Copyright   : (c) K. Isom (2015)
-- License     : BSD-style
--
-- Maintainer  : coder@kyleisom.net
-- Stability   : stable
-- Portability : portable
-- 
-- This is a library for representing and implementing propositional logic
-- proofs. It contains the representations for terms, theorems, and proofs.

module Data.Logic.Propositional.Class where

import Control.Monad.IO.Class
import Control.Monad.Except
import Control.Monad.Trans.Except
import Data.IORef
import qualified Data.Map as Map

-------------------------------------------------------------------------------
------------------------------ Term Definitions -------------------------------
-------------------------------------------------------------------------------

-- | A 'Term' is a single term in the propositional calculus.
data Term = Value Bool
          | Variable String
          | Implies Term Term
          | Conjunction Term Term
          | Disjunction Term Term
          | Equivalence Term Term
          | Negation Term

-- | 'showTerm' returns a 'String' representation of a 'Term'.
showTerm :: Term -> String
showTerm (Value True)      = "t"
showTerm (Value False)     = "f"
showTerm (Variable var)    = var
showTerm (Implies p q)     = (show p) ++ " => " ++ (show q)
showTerm (Conjunction p q) = (show p) ++ " ^ " ++ (show q)
showTerm (Disjunction p q) = (show p) ++ " v " ++ (show q)
showTerm (Equivalence p q) = (show p) ++ " ~ " ++ (show q)
showTerm (Negation p)      =  "!" ++ (show p)

-- | The implementation of the 'Show' typeclass for 'Term' uses
--   'showTerm'.
instance Show Term where show = showTerm

-------------------------------------------------------------------------------
----------------------------------- Errors ------------------------------------
-------------------------------------------------------------------------------

-- | 'Error' represents an error in a Proof.
data Error = Unbound String     -- ^ A variable that should be bound isn't.
           | Rebinding String   -- ^ A variable is being rebound.
           | Inconsistent Term  -- ^ The term is inconsistent.
           | Invalid            -- ^ The proof is invalid.

-- | 'showError' returns a 'String' representation of an 'Error'.
showError :: Error -> String
showError (Unbound name) = "The variable " ++ name ++ " is unbound."
showError (Rebinding name) = "The variable " ++ name ++ " is already bound and can't be rebound."
showError (Inconsistent term) = "The term " ++ (show term) ++ " is inconsistent."
showError Invalid = "The proof evaluated to False."

-- | The implementation of the 'Show' typeclass for 'Error' uses
--   'showError'.
instance Show Error where show = showError

-- | 'ProofError' adds the 'Error' type to the 'IO' monad.
type ProofError = ExceptT Error IO

-- | trapError is used to return a string containing the result of a ProofError.
trapError action = catchError action (return . show)

-------------------------------------------------------------------------------
-------------------------------- Binding -----------------------------------
-------------------------------------------------------------------------------

-- | A binding stores a set of name and value pairings.
data Bindings = Bindings (Map.Map String Bool)

-- | 'newBindings' instantiates a new, empty set of bindings.
newBindings :: Bindings
newBindings = Bindings $ Map.fromList []

-- | 'nullBindings' returns 'True' if there are no bindings.
nullBindings :: Bindings -> Bool
nullBindings (Bindings bindings) = Map.null bindings

-- | 'isBound' returns 'True' if 'name' is bound.
isBound :: Bindings -> String -> Bool
isBound (Bindings bindings) name = Map.member name bindings

-- | 'getName' returns the binding for 'name'.
getName :: Bindings -> String -> ProofError Term
getName b@(Bindings bindings) name = do
  if isBound b name
     then return $ Value $ bindings Map.! name
     else throwError $ Unbound name

-- | 'bindName' binds 'val' to 'name'.
bindName :: Bindings -> String -> Bool -> ProofError Bindings
bindName b@(Bindings bindings) name val = case isBound b name of
    True  -> throwError $ Rebinding name
    False -> return $ Bindings $ Map.insert name val bindings

-- | 'showPair' returns a string representation of a binding.
showPair :: (String, Bool) -> String
showPair (name, val) = name ++ " <- " ++ (show val)

-- | 'showBindings' returns the bindings as a string.
showBindings :: Bindings -> String
showBindings (Bindings bindings) = unlines . map showPair $ Map.toAscList bindings

-- | The 'Show' typeclass is provided using 'showBindings'.
instance Show Bindings where show = showBindings

-------------------------------------------------------------------------------
------------------------------ Term Evaluation --------------------------------
-------------------------------------------------------------------------------

-- | A theorem is either an axiom (which sets up a binding for the name), or a
--   theorem, which contains a term.
data Theorem = Axiom String Bool
             | Theorem Term

-- | 'showTheorem' returns a 'String' representation for a 'Theorem'.
showTheorem :: Theorem -> String
showTheorem (Axiom name val) = "Axiom: " ++ name ++ " ← " ++ show val
showTheorem (Theorem term) = "Theorem: " ++ show term

-- | The implementation of the 'Show' typeclass for 'Theorem' uses
--   'showTheorem'.
instance Show Theorem where show = showTheorem

-- | A 'Proof' contains a set of bindings and a sequence of theorems.
data Proof = Proof Bindings [Theorem]

-- | 'showProof' returns a 'String' representation of a 'Proof'.
showProof :: Proof -> String
showProof p@(Proof b thms)
  | nullBindings b = showProofTheorems p
  | True           = showProofBindings p

-- | 'showProofTheorems' returns a string representation of only the
--   proofs in a 'Proof'. This is called when there are no bindings
--   in the proof to be shown.
showProofTheorems (Proof _ [])   = "null proof"
showProofTheorems (Proof _ thms) = "Proof:\n" ++ (unlines $ map show thms)

-- | 'showProofBindings' returns a string representation of the proof
--   and its bindings.
showProofBindings (Proof b [])   = "Null proof; bindings:\n" ++ showBindings b
showProofBindings (Proof b thms) = "Proof:\n" ++ (unlines $ map show thms)
                                      ++ "bindings:\n" ++ showBindings b

instance Show Proof where show = showProof
