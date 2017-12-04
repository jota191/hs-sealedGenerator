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

module Parser where
import ParserCombinators
import Model
import Data.Char
import Prelude hiding ((<|>),(<$>),(<*>))

-- | Parses a character, from a name
pAlphaSpace :: Parser String Char
pAlphaSpace = Parser $ \s -> case s of
                               (c:cs) -> if isAlphaNum c
                                            || c ==' '
                                            || c =='\''
                                            || c == '!'
                                            || c == '-'
                                            || c == ':'
                                            || c == '.'
                                         then [(c,cs)]
                                         else []
                               []     -> []

pQuotation = Parser $ \s -> case s of
                              (c:cs) -> if c =='\"'
                                        then [(c,cs)]
                                        else []
                              []     -> []


-- | Parses a Phrase (a word, with ANY spacers)
pPhrase :: Parser String String
pPhrase =  pList pAlphaSpace
       
-- | Parses a Name
pName = pPhrase
     <|> ((\_ l c r _ -> l++[c]++r) <$>
          pQuotation <*> pPhrase <*> pComma <*> pPhrase <*> pQuotation)

-- | Parses a Rarity
pRarity =   pToken "Basic"     Basic_R
        <|> pToken "Common"    Common
        <|> pToken "Rare"      Rare
        <|> pToken "Epic"      Epic
        <|> pToken "Legendary" Legendary


-- | Parses an expansion Set 
pSet =  pToken "Promo"     Promo
    <|> pToken "Basic"     Basic_E
    <|> pToken "Classic"   Classic
    <|> pToken "Naxx"      Naxx
    <|> pToken "GvG"       GvG
    <|> pToken "Blackrock" Blackrock
    <|> pToken "TGT"       TGT
    <|> pToken "LoE"       LoE
    <|> pToken "TOG"       TOG
    <|> pToken "Kara"      Kara
    <|> pToken "MSG"       MSG
    <|> pToken "Un'Goro"   UnGoro 
    <|> pToken "KFT"       KFT

-- | Parses a Class
pClass =   pToken "Neutral" Neutral
      <|> pToken "Druid"    Druid
      <|> pToken "Hunter"   Hunter
      <|> pToken "Mage"     Mage
      <|> pToken "Paladin"  Paladin
      <|> pToken "Priest"   Priest
      <|> pToken "Rogue"    Rogue
      <|> pToken "Shaman"   Shaman
      <|> pToken "Warlock"  Warlock
      <|> pToken "Warrior"  Warrior

-- | Parses a separator, commas by now, another kind of char
--   if we want to support any other formats (csv by now)
pSeparator = pComma

pComma = Parser $ \s -> case s of
                          (c:cs) -> if c == ','
                                    then [(c,cs)]
                                    else []
                          [] -> []


pEOL = Parser $ \s -> case s of
                        (c:cs) -> if c == '\n'
                                  then [(c,cs)]
                                  else []
                        [] -> []


pQuantity :: Parser String Int
pQuantity = Parser $ \s -> case s of
                             (c:cs) -> if   isNumber c
                                       then [(read [c] :: Int,cs)]
                                       else []
                             [] -> []


-- | Finally, this Parses a Card
pCard :: Parser String (Card,Int)
pCard = (\name _ rarity _ set _ clss _ normal _ golden _
         -> (Card name clss rarity set, if normal+golden > 2
                                        then 2
                                        else normal+golden))
        <$> pName     <*> pSeparator
        <*> pRarity   <*> pSeparator
        <*> pSet      <*> pSeparator
        <*> pClass    <*> pSeparator
        <*> pQuantity <*> pSeparator
        <*> pQuantity <*> pEOL

pCollection :: Parser String [(Card,Int)]
pCollection = pList pCard
