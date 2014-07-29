{-# LANGUAGE DeriveDataTypeable #-}
module AI where



import Data.Bits
import Data.Maybe
import Control.Monad 

import System.Timeout
import Control.Exception
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

gameEnd :: CBoard -> Bool
gameEnd board = do
  let ms1 = validMovesC board black
      ms2 = validMovesC board white
   in null ms1 && null ms2

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

weightPop :: CBoard -> Color -> Int
weightPop (CBoard bl wh) color =
  let bp = if color == black then bl else wh
      ls = filter ( \(i, j) -> bp .&. (1 `shiftL` (i + 8*j - 9)) /= 0) [(i,j) | i <- [1..8], j <- [1..8]]
   in sum $ map weightOfPlace ls
-- | timeout : timeout in microseconds (us)
myPlay :: CBoard -> Color -> Heuristics -> Int -> IO Mv 
myPlay board color mode time = do
    let ms = validMovesC board color in
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
       vals <- forM boards $ \(mv, bd) -> do
         valPath <- alphaBeta mode bd depth color (oppositeColor color) minValue maxValue numBoards
         return (mv, valPath)
       let valsSort = sortBy (flip (compare `on` (fst . snd))) vals
       -- print valsSort -- sort by value, largest first
       -- putStrLn ""
       writeIORef boardsRef $ map (\(mv, _) -> (mv, fromMaybe (error "(>_<)") $ lookup mv boards)) valsSort
       let ((i, j), (optval, path)) = if null vals then undefined else head valsSort
       printf "depth = %d, move = (%d, %d), value = %d\n" depth i j optval :: IO ()
       putStrLn $ "path = " ++ show (fmap (M i j :) path)
       newNum <- readIORef numBoards
       printf "number of boards in depth %d: %d\n" depth (newNum - oldNum) :: IO ()
       writeIORef opt $ Just (M i j : fromMaybe [] path, optval) -- path is Just _

alphaBeta :: Heuristics -> CBoard -> Int -> Color -> Color -> Int -> Int -> IORef Int -> IO (Int, Maybe [Mv])
alphaBeta mode board depth mycol curcol alpha beta numBoards = do
  let isGameEnd = gameEnd board
  if isGameEnd || depth == 0 then do
     let result = staticEval board mycol mode isGameEnd
     modifyIORef numBoards (+1)
     return (result, Just [])
  else if curcol == mycol then do
    aref <- newIORef (alpha, Nothing)
    let ms = validMovesC board curcol
    let moves = if null ms then [Pass] else map (\(i,j) -> M i j) ms
    forM_ moves $ \m -> do
      (calpha, _) <- readIORef aref
      if calpha >= beta then do
         writeIORef aref (beta, Nothing)
      else do 
        let nxt = doMoveC board m curcol
        let nextp = oppositeColor curcol
        (result, path) <- alphaBeta mode nxt (depth - 1) mycol nextp calpha beta numBoards
        modifyIORef aref (maxBy (compare `on` fst) (result, fmap (m :) path))
    readIORef aref
  else do
    bref <- newIORef (beta, Nothing)
    let ms = validMovesC board curcol
    let moves = if null ms then [Pass] else map (\(i,j) -> M i j) ms
    forM_ moves $ \m -> do
      (cbeta, _) <- readIORef bref
      if alpha >= cbeta then do
         writeIORef bref (alpha, Nothing)
      else do 
        let nxt = doMoveC board m curcol
        let nextp = oppositeColor curcol
        (result, path) <- alphaBeta mode nxt (depth - 1) mycol nextp alpha cbeta numBoards
        modifyIORef bref (minBy (compare `on` fst) (result, fmap (m :) path))
    readIORef bref

staticEval :: CBoard -> Color -> Heuristics -> Bool -> Int
staticEval board color mode isGameEnd = 
  if isGameEnd then
    let my = countC board color 
        opp = countC board (oppositeColor color) in
        case compare my opp of
          LT -> loseValue + my - opp
          EQ -> drawValue
          GT ->  winValue + my - opp
  else
    ([eval1, eval2, eval3, eval4, eval5] !! (mode - 1)) board color

{- The number of disks -}
eval1 :: CBoard -> Color -> Int
eval1 board color =
    let my = countC board color 
        opp = countC board (oppositeColor color) in
    my - opp

eval2 :: CBoard -> Color -> Int
eval2 board color =
  let opp = oppositeColor color
      ms  = validMovesC board opp in
  - (length ms)

eval3 :: CBoard -> Color -> Int
eval3 board color =
  let opp = oppositeColor color
      blc = countC board black
      whc = countC board black
      msmy = validMovesC board color
      msopp = validMovesC board opp
      iv = countC board color in
  if blc + whc >= 50 then iv - (blc + whc) else length msmy - length msopp

eval4 :: CBoard -> Color -> Int
eval4 board color =
  let opp = oppositeColor color
      blc = countC board black
      whc = countC board black
      msmy = validMovesC board color
      msopp = validMovesC board opp
      iv = countC board color in
  if blc + whc >= 50 then iv - (blc + whc) else sum (map weightOfPlay msmy) - sum (map weightOfPlay msopp)

eval5 :: CBoard -> Color -> Int
eval5 board color =
  let opp = oppositeColor color
      msmy = validMovesC board color
      msopp = validMovesC board opp
      myWpop = weightPop board color
      oppWpop = weightPop board (oppositeColor color) in
  myWpop - oppWpop + sum (map weightOfPlay msmy) - sum (map weightOfPlay msopp)

