module Main where

import DeBruijn
import Parsing
import NanoParsec

import Control.Monad

eval :: Term -> IO Term
eval x = do
  let x' = reduce x
  if x' == x then return x
  else do
    print x'
    eval x'

main :: IO ()
main = forever $ do
  a <- getLine
  let expr = runParser term a 
  eval expr
