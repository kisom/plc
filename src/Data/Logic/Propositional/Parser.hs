-- |
-- Module      : Data.Logic.Propositional.Parser
-- Copyright   : (c) K. Isom (2015)
-- License     : BSD-style
--
-- Maintainer  : coder@kyleisom.net
-- Stability   : stable
-- Portability : portable
-- 
-- This is a library for representing and implementing propositional
-- logic proofs. This module contains the parser for converting string
-- representations into theorems.

module Data.Logic.Propositional.Parser where

import Data.Logic.Propositional.Class

import Control.Applicative
import Control.Monad.Except

import qualified Data.Char
import qualified Text.ParserCombinators.Parsec as P
import qualified Text.ParserCombinators.Parsec.Expr as E

spaces :: P.Parser ()
spaces = P.skipMany P.space

parseTruth :: P.Parser Bool
parseTruth = do
  hash <- P.char '#'
  val  <- P.oneOf "fFtT"
  let value = [hash, Data.Char.toUpper val]
  return $ case value of
      "#T" -> True
      "#F" -> False

parseValue :: P.Parser Term
parseValue = parseTruth >>= return . Value

parseName :: P.Parser String
parseName = (:) <$> P.letter <*> P.many (P.letter <|> P.digit)

parseVariable :: P.Parser Term
parseVariable = parseName >>= return . Variable

expr :: P.Parser Term
expr = E.buildExpressionParser termTable term

termTable :: [[E.Operator Char st Term]]
termTable = [
     [E.Prefix (P.string "!" >> return Negation)]
    ,[binOp "&"  Conjunction]
    ,[binOp "|"  Disjunction]
    ,[binOp "->" Implies]
    ,[binOp "~"  Equivalence]]
  where binOp s f = E.Infix (P.string s >> return f) E.AssocLeft

term = 
  P.between spaces spaces $ parseValue
  <|> parseVariable
  <|> P.between (P.char '(') (P.char ')') expr

parseLemma :: String -> ProofError Theorem
parseLemma s = case P.parse expr "theorem-parser" s of
    Left err  -> throwError $ Syntax $ show err
    Right val -> return $ Theorem val

axiom :: P.Parser Theorem
axiom = do
  name <- parseName
  skipped <- spaces >> P.string ":=" >> spaces
  value <- parseTruth
  return $ Axiom name value
  
parseAxiom :: String -> ProofError Theorem
parseAxiom s = case P.parse axiom "axiom-parser" s of
    Left err  -> throwError $ Syntax $ show err
    Right val -> return $ val

readExpr :: String -> ProofError Theorem
readExpr s = case head tokens of
  "axiom:"   -> parseAxiom body
  "theorem:" -> parseLemma body
  _          -> throwError $ Syntax s
  where tokens = words s
        body   = unwords $ tail tokens

showParsed :: ProofError Theorem -> IO ()
showParsed thm = do
  result <- runExceptT thm
  case result of
    Left err   -> putStrLn $ show err
    Right thm' -> putStrLn $ show thm'
