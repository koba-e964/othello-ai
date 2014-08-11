{-# OPTIONS -fno-warn-unused-imports #-} -- in order to automatically load AI when this is loaded in GHCi
module Exam where

import Control.Monad
import Data.Array.IO
import Data.Bits
import Data.List

import Color
import Common
import CBoard
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

readBoard :: String -> IO CBoard
readBoard str = do
    let lns = map (filter (\x -> elem x "xoXO.")) $ lines str
    let rows = [(i, lns !! i) | i <- [0 .. min 8 (length lns) - 1]]
    board <- newArray ((0,0), (7,7)) none :: IO (IOArray (Int,Int) Int)
    forM_ rows $ \(rn, row) -> do
        let cols = [(i, row !! i) | i <- [0 .. min 8 (length row) - 1]]
        forM_ cols $ \(cn, colc) -> writeArray board (cn, rn) (charToColor colc)
    let trans = map $ \(i,j) -> 1 <<< (i + 8 * j) :: Places
    bls <- fmap trans $ flip filterM [(i,j) | i <- [0..7], j <- [0..7]] $ \(i,j) -> do
        el <- readArray board (i,j)
        return $ el == black
    whs <- fmap trans $ flip filterM [(i,j) | i <- [0..7], j <- [0..7]] $ \(i,j) -> do
        el <- readArray board (i,j)
        return $ el == white
    return $ CBoard (foldl' (.|.) 0 bls) (foldl' (.|.) 0 whs)
       where
         charToColor 'x' = black
         charToColor 'X' = black
         charToColor 'o' = white
         charToColor 'O' = white
         charToColor '.' = none
         charToColor _   = undefined

-- | getBoard reads 8 lines and convert them to a board.

getBoard :: IO CBoard
getBoard = do
  str <- replicateM 8 getLine
  readBoard (intercalate "\n" str)

