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

module Main where
import Parser
import ParserCombinators
import Model
import System.Random

main = do file   <- readFile "../HMCData/Lando.csv"
          col    <- return $ fst $ (runP pCollection file)!!0
          kftcol <- return $ map fst $
            filter (\(card,n) -> set card == KFT && n/=0) col
          (Splitted c r e l) <- return $ splitByRarity kftcol
          rng <- ((flip mod) 100) <$> (randomIO :: IO Int)
          do if rng < 95
               then do c <- getRandomFromList r
                       printCard c
               else if rng < 99
                    then do c <- getRandomFromList e
                            printCard c
                    else do c <- getRandomFromList l
                            printCard c
          selectCard (Splitted c r e l)
          selectCard (Splitted c r e l)
          selectCard (Splitted c r e l)
          selectCard (Splitted c r e l)
          

selectCard :: Splitted -> IO ()
selectCard (Splitted c r e l)
  = do rng <- ((flip mod) 100) <$> (randomIO :: IO Int)
       do if rng < 70
            then do c <- getRandomFromList c
                    printCard c
            else if rng < 95
            then do c <- getRandomFromList r
                    printCard c
            else if rng < 99
                 then do c <- getRandomFromList e
                         printCard c
                 else do c <- getRandomFromList l
                         printCard c
          
exp = KFT

getRandomFromList :: [a] -> IO a
getRandomFromList l = do rng1 <- randomIO :: IO Int
                         index <- return $ rng1 `mod` (length l)
                         return (l!!index)
