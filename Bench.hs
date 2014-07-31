module Main where

import Common
import CBoard
import AI
import Control.Monad
import Data.Bits
import System.CPUTime
import System.Environment
import System.Random
import Text.Printf

test :: Places -> Int
test param = eval5 param ((param * 0x123456789) &&& (complement param))

test2 :: Places -> Places
test2 param = validMovesSetMO param ((param * 0x123456789) &&& (complement param))


bench :: Int -> IO Integer
bench times = do
  start <- getCPUTime
  replicateM_ times $ do
    param <- randomIO :: IO Places
    let x = test param
    return $! x
  end <- getCPUTime
  return $ end - start
main :: IO ()
main = do
  args <- getArgs
  let times = if null args then 1000000 else read (head args)
  result <- bench times
  printf "#iteration : %d, time : %d us\n" times (result `div` (10^(6 :: Int))) :: IO ()

