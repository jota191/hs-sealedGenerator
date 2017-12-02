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


pAlphaSpace :: Parser String Char
pAlphaSpace = Parser $ \s -> case s of
                               (c:cs) -> if isAlpha c || c == ' '
                                         then [(c,cs)]
                                         else []
                               []     -> []

pPhrase :: Parser String String
pPhrase = pList pAlphaSpace


pName = pPhrase
-- TODO: this should return a Rarity or fail, not a List
pRarity :: Parser String [Rarity]
pRarity = (wrap . readRarity) <$> pPhrase
  where wrap Nothing  = []
        wrap (Just a) = [a]

pRarity' :: Parser String Rarity
pRarity' = Parser $ \s  -> do xs <- runP pPhrase s 
                              case readRarity xs of
                                Nothing -> undefined --pFail
                                Just a  -> return a

--pSet :: Parser String [Set]

readRarity :: String -> Maybe Rarity
readRarity "Basic"     = Just Basic_R
readRarity "Common"    = Just Common
readRarity "Rare"      = Just Rare
readRarity "Epic"      = Just Epic
readRarity "Legendary" = Just Legendary
readRarity _           = Nothing

readSet :: String -> Maybe Set
readSet "Promo"     = Just Promo
readSet "Basic"     = Just Basic_E
readSet "Classic"   = Just Classic
readSet "Naxx"      = Just Naxx
readSet "GvG"       = Just GvG
readSet "Blackrock" = Just Blackrock
readSet "TGT"       = Just TGT
readSet "LoE"       = Just LoE
readSet "TOG"       = Just TOG
readSet "Kara"      = Just Kara
readSet "MSG"       = Just MSG
readSet "Un'Goro"   = Just UnGoro 
readSet "KFT"       = Just KFT
readSet _           = Nothing
