{-# LANGUAGE BangPatterns, DeriveDataTypeable #-}
module AI where



import Data.Bits
import Data.Maybe
import Control.Monad 

import System.Timeout
import Control.Exception
import Data.Array.Unboxed
import Data.Function
import Data.List
import Data.IORef
import Data.Typeable
import Text.Printf

import Color 
import Command
import Common
import CBoard

type Heuristics = Int

winValue :: Int
loseValue :: Int
drawValue :: Int
maxValue :: Int
minValue :: Int

winValue  =  10000000
loseValue = -10000000
drawValue = - 5000000
maxValue  =  100000000
minValue  = -100000000

data StopSearch = StopSearch String deriving (Show, Typeable)

instance Exception StopSearch 

-- | These functions prefer the second value if two equal.
maxBy :: (a -> a -> Ordering) -> a -> a -> a
minBy :: (a -> a -> Ordering) -> a -> a -> a

maxBy comp x y = case comp x y of
   GT -> x
   _  -> y

minBy comp x y = case comp x y of
   LT -> x
   _  -> y

gameEnd :: Places -> Places -> Bool
gameEnd my opp = do
  let ms1 = validMovesSetMO my opp
      ms2 = validMovesSetMO opp my
   in ms1 == 0 && ms2 == 0

weightOfPlay :: (Int, Int) -> Int
weightOfPlay (i, j) = sub (if i <= 4 then i else 9-i) (if j <= 4 then j else 9-j) where
  sub 1 1 = 1000
  sub 1 2 = 2
  sub 2 1 = 2
  sub 2 2 = 1
  sub 1 3 = 4
  sub 3 1 = 4
  sub _ _ = 5
  
weightOfPlace :: (Int, Int) -> Int
weightOfPlace (i, j) = sub (if i <= 4 then i else 9-i) (if j <= 4 then j else 9-j) where
  sub 1 1 = 100
  sub 1 2 = -2
  sub 2 1 = -2
  sub 2 2 = -6
  sub 1 3 = -1
  sub 3 1 = -1
  sub _ _ = 0
wpPlayTbl :: UArray (Int,Int) Int
wpPlayTbl = array ((0,0),(7,255)) [((i-1,j), sum $ map (\k -> weightOfPlay (i,k+1)) $ filter (testBit j) [0..7]) | i <- [1..8], j <- [0..255]]
weightPlayPop :: Places -> Int
weightPlayPop places =
    sum $ [wpPlayTbl ! (i, fromIntegral ((places `shiftR` (8*i)) .&. 0xff)) | i <- [0..7]]

wpTbl :: UArray (Int,Int) Int
wpTbl = array ((0,0),(7,255)) [((i-1,j), sum $ map (\k -> weightOfPlace (i,k+1)) $ filter (testBit j) [0..7]) | i <- [1..8], j <- [0..255]]
weightPop :: Places -> Int
weightPop bp =
    sum $ [wpTbl ! (i, fromIntegral ((bp `shiftR` (8*i)) .&. 0xff)) | i <- [0..7]]
-- | timeout : timeout in microseconds (us)
myPlay :: CBoard -> Color -> Heuristics -> Int -> IO Mv 
myPlay board color mode time = do
    -- let ms = validMovesC board color in
    let ms = placesToPositions $ validMovesSet board color in 
       case ms of 
         [] -> return Pass
         _  -> 
             do
                boards <- newIORef $ map (\mv@(mvi, mvj) -> (mv, doMoveC board (M mvi mvj) color)) ms
                opt <- newIORef (Nothing :: Maybe ([Mv], Int))
                numBoards <- newIORef 0 :: IO (IORef Int)
                _ <- timeout time $ catch (
                  forM_ [0..] (nextMoveDepth board boards color mode opt numBoards)
                 ) (\(StopSearch str) -> putStrLn str) -- this always fails and opt is modified with the result
                optmv <- readIORef opt
                numBoardsVal <- readIORef numBoards
                printf "summary:\n depth : %d\n total #boards: %d\n" (fromMaybe (-1) (fmap (length . fst) optmv)) numBoardsVal :: IO ()
                let mv = case optmv of { Nothing -> ( \(i,j) -> M i j) (head ms); Just (o:_, _) -> o; _ -> undefined; }
                return mv;

-- overwrites opt and stort the optimal value
-- boards is rearranged after operation
nextMoveDepth :: CBoard -> IORef [((Int, Int), CBoard)] -> Color -> Heuristics -> IORef (Maybe ([Mv], Int)) -> IORef Int -> Int -> IO ()
nextMoveDepth _board boardsRef color mode opt numBoards depth = do
       curopt <- readIORef opt
       boards <- readIORef boardsRef
       oldNum <- readIORef numBoards
       case curopt of
         Nothing -> return ()
         Just (_, curoptval) -> do
           when (curoptval >= winValue) $ throwIO (StopSearch $ printf "Path to the victory was detected. (depth = %d)" (depth - 1))
           when (curoptval <= loseValue) $ throwIO (StopSearch $ printf "Path to the defeat was detected. (depth = %d)" (depth - 1))
       vals <- forM boards $ \(mv, CBoard bdbl bdwh) -> do
         let (!my, !opp) = if color == black then (bdbl,bdwh) else (bdwh, bdbl)
         valPath <- alphaBeta mode opp my depth minValue maxValue numBoards True
         return (mv, valPath)
       let valsSort = sortBy (compare `on` (fst . snd)) vals
       -- print valsSort -- sort by value, largest first
       -- putStrLn ""
       writeIORef boardsRef $ map (\(mv, _) -> (mv, fromMaybe (error "(>_<)") $ lookup mv boards)) valsSort
       let ((i, j), (optval, path)) = if null vals then undefined else head valsSort
       let wholePath = M i j : map conv (fromMaybe [] path) -- path is Just _
       printf "depth = %d, move = (%d, %d), value = %d\n" depth i j (-optval) :: IO ()
       putStrLn $ "path = " ++ show wholePath
       newNum <- readIORef numBoards
       printf "number of boards in depth %d: %d\n" depth (newNum - oldNum) :: IO ()
       writeIORef opt $ Just (wholePath, -optval)
        where
          conv 0 = Pass
          conv x = let t = popCount (x-1) in M (t `mod` 8 + 1) (t `div` 8 + 1)

alphaBeta :: Heuristics -> Places -> Places -> Int -> Int -> Int -> IORef Int -> Bool -> IO (Int, Maybe [Places])
alphaBeta mode my opp depth alpha beta numBoards isOpp = do
  let isGameEnd = gameEnd my opp
  if isGameEnd || depth == 0 then do
     let result = staticEval my opp mode isGameEnd isOpp
     modifyIORef numBoards (+1)
     return (result, Just [])
  else do
    aref <- newIORef (alpha, Nothing)
    let ms = validMovesSetMO my opp
    if ms == 0 then do
        (result, path) <- alphaBeta mode opp my (depth - 1) (-beta) (-alpha) numBoards (not isOpp)
        modifyIORef aref (maxBy (compare `on` fst) (-result, fmap (0 :) path))
    else
     forM_ (setToDisks ms) $ \disk -> do
      (calpha, _) <- readIORef aref
      if calpha >= beta then do
         writeIORef aref (beta, Nothing)
      else do 
        let (nxtopp, nxtmy) = doMoveBit my opp disk
        (result, path) <- alphaBeta mode nxtmy nxtopp (depth - 1) (-beta) (-calpha) numBoards (not isOpp)
        modifyIORef aref (maxBy (compare `on` fst) (-result, fmap (disk :) path))
    readIORef aref

staticEval :: Places -> Places -> Heuristics -> Bool -> Bool -> Int
staticEval my opp mode isGameEnd isOpp = 
  if isGameEnd then
    let myc = popCount my 
        oppc = popCount opp in
        case compare myc oppc of
          LT -> loseValue + myc - oppc
          EQ -> if isOpp then -drawValue else drawValue -- in order to make value negative whether my represents places of disks of mine or opponent's.
          GT ->  winValue + myc - oppc
  else
    ([eval1, eval2, eval3, eval4, eval5] !! (mode - 1)) my opp

{- The number of disks -}
eval1 :: Places -> Places -> Int
eval1 my opp =
    let my_ = popCount my 
        opp_ = popCount opp in
    my_ - opp_

eval2 :: Places -> Places -> Int
eval2 my opp =
  let ms  = validMovesSetMO opp my in
  - popCount ms

eval3 :: Places -> Places -> Int
eval3 my opp =
  let myc = popCount my
      oppc = popCount opp
      msmy = validMovesSetMO my opp
      msopp = validMovesSetMO opp my
     in
  if myc + oppc >= 50 then -oppc else popCount msmy - popCount msopp

eval4 :: Places -> Places -> Int
eval4 my opp =
  let !myc = popCount my
      !oppc = popCount opp
      !msmy = validMovesSetMO my opp
      !msopp = validMovesSetMO opp my
    in
  if myc + oppc >= 50 then -oppc else weightPlayPop msmy - weightPlayPop msopp

eval5 :: Places -> Places -> Int
eval5 my opp =
  let !msmy = validMovesSetMO my opp
      !msopp = validMovesSetMO opp my
      !myWpop = weightPop my
      !oppWpop = weightPop opp in
  myWpop - oppWpop + weightPlayPop msmy - weightPlayPop msopp

