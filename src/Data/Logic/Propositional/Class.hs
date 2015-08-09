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

-- | 'showError' returns a 'String' representation of an 'Error'.
showError :: Error -> String
showError (Unbound name) = "The variable " ++ name ++ " is unbound."
showError (Rebinding name) = "The variable " ++ name ++ " is already bound and can't be rebound."
showError (Inconsistent term) = "The term " ++ (show term) ++ " is inconsistent."

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
type Bindings = IORef [(String, Bool)]

-- | 'nullBindings' is a helper function to create a new, empty set
--   of bindings.
nullBindings :: IO Bindings
nullBindings = newIORef []

-- | 'isBound' returns 'True' if 'name' is present in the set of bindings.
isBound :: Bindings -> String -> IO Bool
isBound bindings name = readIORef bindings >>= return . maybe False (const True) . lookup name

-- | 'getName' returns the binding for a name in the set of bindings.
getName :: Bindings -> String -> ProofError Bool
getName bindings name = do
    bindings' <- liftIO $ readIORef bindings
    maybe (throwError $ Unbound name)
          (return . id)
          (lookup name bindings')

-- | 'bindName' adds a binding to the set.
bindName :: Bindings -> String -> Bool -> ProofError Bool
bindName bindings name val = do
    bindings' <- liftIO $ readIORef bindings
    exists    <- liftIO $ isBound bindings name
    case exists of
        True  -> throwError $ Rebinding name
        False -> do
            val' <- liftIO $ writeIORef bindings ((name, val) : bindings')
            return val

showPair :: String -> Bool -> String
showPair name val = name ++ " <- " ++ (show val)

showBindings_ :: [(String, Bool)] -> IO ()
showBindings_ ((name, val):rest) = putStrLn (showPair name val) >> showBindings_ rest
showBindings_ [] = return ()

showBindings :: Bindings -> IO ()
showBindings bindings = putStrLn "Bindings:" >> readIORef bindings >>= showBindings_

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
type Proof = (IO Bindings, [Theorem])

instance Show Proof where show _ = "<proof>"
