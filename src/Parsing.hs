module Parsing where

import NanoParsec
import DeBruijn

import Control.Applicative

index :: Parser Index
index = (toEnum . fromIntegral) <$> natural <|> lambda
  where
    lambda = char '\\' >> return Z

term :: Parser Term
term = (I <$> index) <|> parens pair
  where 
    pair = do
      l <- token term
      r <- token term
      return (P l r)


