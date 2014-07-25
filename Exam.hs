module Exam where

import Control.Monad
import Data.Array.IO

import Color
import Play
import AI


-- | readBoard creates a board from given String.
-- | e.g. 
-- |  readBoard "....x.x. \nox..xo.." returns a board representing
-- |  ....X.X.
-- |  OX..XO..
-- |  ........
-- |  ........
-- |  ........
-- |  ........
-- |  ........
-- |  ........

readBoard :: String -> IO Board
readBoard str = do
     let lns = map (filter (\x -> elem x "xoXO.")) $ lines str
     let rows = [(i + 1, lns !! i) | i <- [0 .. min 8 (length lns) - 1]]
     board <- newArray ((0,0), (9,9)) none
     mapM_ (\i ->
               do writeArray board (i,0) sentinel 
                  writeArray board (i,9) sentinel
                  writeArray board (0,i) sentinel 
                  writeArray board (9,i) sentinel) [0..9]
     forM_ rows $ \(rn, row) -> do
         let cols = [(i + 1, row !! i) | i <- [0 .. min 8 (length row) - 1]]
         forM_ cols $ \(cn, colc) -> writeArray board (cn, rn) (charToColor colc)
     return board
       where
         charToColor 'x' = black
         charToColor 'X' = black
         charToColor 'o' = white
         charToColor 'O' = white
         charToColor '.' = none

