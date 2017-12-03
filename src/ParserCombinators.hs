{-
Copyright (c) 2017 Juan García Garland

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

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

