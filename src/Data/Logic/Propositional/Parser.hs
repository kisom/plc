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

parseSet :: P.Parser [Char]
parseSet = P.string ":="

parseName :: P.Parser String
parseName = (:) <$> P.letter <*> P.many (P.letter <|> P.digit)

parseVariable :: P.Parser Term
parseVariable = parseName >>= return . Variable
