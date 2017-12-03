{-
MIT License

Copyright (c) 2016,2017 Juan García Garland

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
-}


{-# LANGUAGE InstanceSigs #-}

module ParserCombinators (Parser(Parser,runP),
                          pFail,
                          pSucceed,
                          pList,
                          pToken,
                          (<|>),(<$>),(<*>))
where

import Data.List
import Data.Maybe

import Control.Monad
import Control.Applicative

-- | Datatype for parsers
data Parser s a = Parser {runP :: s -> [(a,s)]}


-- | We will use monadic parsers
instance Monad (Parser s) where
  return a = Parser $ \s -> [(a,s)]
  p >>= q  = Parser $ \s -> concat [runP (q a) s' | (a,s') <- runP p s]


--Parser Combinators

pFail :: Parser s a
pFail = Parser $ \s -> []


pSucceed :: a -> Parser s a
pSucceed a = Parser $ \s -> [(a,s)]

instance Alternative (Parser s) where
  (<|>) :: Parser s a -> Parser s a -> Parser s a
  p <|> q = Parser $ \s -> runP p s ++ runP q s
  empty   =  Parser $ \char -> empty



instance Functor (Parser s) where
  fmap :: (a -> b) -> Parser s a -> Parser s b
  fmap = liftM

instance Applicative (Parser s) where
  (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
  (<*>) = ap
  pure = return

pList :: Parser s a -> Parser s [a]
pList p = (:) <$> p <*> pList p
       <|> pSucceed [] 

-- | Parses the String in the first parameter, returning the second
pToken :: String -> a -> Parser String a
pToken lexeme token
  = Parser $ \s -> if isPrefixOf lexeme s
                   then [(token,fromJust (stripPrefix lexeme s))]
                   else []

