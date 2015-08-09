module Main where

import Data.Logic.Propositional 
import Data.Logic.Propositional.Class

testThm = Theorem (Conjunction (Variable "A") (Negation (Variable "A")))

main = (putStrLn "Test theorem:") >> (putStrLn $ show testThm)
