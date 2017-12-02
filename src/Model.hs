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


module Model where

data Expansion = Promo | Basic_E    | Classic | Naxx
               | GvG   | Blackrock  | TGT     | LoE
               | TOG   | Kara       | MSG     | UnGoro -- written Un'Goro
               | KFT
               deriving (Eq,Ord,Show)
{-
Ord instance will have sense since the last one is the most recent,
except for Promo, Show could be implemented better if required
-}

data Class = Neutral | Druid | Hunter | Mage    | Paladin
           | Priest  | Rogue | Shaman | Warlock | Warrior
           deriving (Eq, Show)


data Rarity = Basic_R | Common | Rare | Epic | Legendary
            deriving (Show,Eq)

data Card = Card {  name      :: String,
                    clss      :: Class,
                    rarity    :: Rarity,
                    expansion :: Expansion
                 }

type Collection = [Card] 
