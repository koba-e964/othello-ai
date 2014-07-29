module Common where

import Data.Array.IO
import Data.Word

-- | A set of places in reversi board.

type Places = Word64

-- 番兵付きの10x10配列
type Board = IOUArray (Int,Int) Int 

-- | A efficient data structure for states of reversi board.
-- | each bit corresponds to a cell in the board:
-- |  0  1  2  3  4  5  6  7
-- |  8  9 10 11 12 13 14 15
-- | ...
-- | 56 57 58 59 60 61 62 63
-- | (i,j) <====> 8*(j-1)+(i-1)
-- | the first Places represents the positions of black disks.
-- | the second Places represents the positions of white disks.
data CBoard = CBoard !Places !Places


