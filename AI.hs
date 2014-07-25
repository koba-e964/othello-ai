{-# LANGUAGE DeriveDataTypeable #-}
module AI where



import Data.Array.IO
import Data.Array.MArray
import Data.List ((\\))
import Data.Maybe
import Control.Monad 

import System.Random
import System.Timeout
import Control.Exception
import Data.Function
import Data.List
import Data.IORef
import Data.Typeable
import Text.Printf

import Color 
import Command
import Play

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

gameEnd :: Board -> IO Bool
gameEnd board = do
  ms1 <- validMoves board black
  ms2 <- validMoves board white
  return $ null ms1 && null ms2

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

weightPop :: Board -> Color -> IO Int
weightPop board color = do
  ls <- filterM ( \(i, j) -> do
     c <- readArray board (i, j)
     return $ c == color
   ) [(i,j) | i <- [1..8], j <- [1..8]]
  return $ sum $ map weightOfPlace ls
-- | timeout : timeout in microseconds (us)
myPlay :: Board -> Color -> Heuristics -> Int -> IO Mv 
myPlay board color mode time =
    do ms <- validMoves board color 
       case ms of 
         [] -> return Pass
         _  -> 
             do
                boards <- newIORef =<< mapM (\mv@(mvi, mvj) -> fmap (\x ->(mv,x)) (doMoveCopy board (M mvi mvj) color)) ms
                blc <- count board black
                whc <- count board black
                opt <- newIORef (Nothing :: Maybe (Mv, Int))
                timeout time $ catch (
                  forM_ [0..] (nextMoveDepth board boards color mode opt)
                 ) (\(StopSearch str) -> putStrLn str)
                optmv <- readIORef opt
                let mv = case optmv of { Nothing -> ( \(i,j) -> M i j) (head ms); Just (o, _) -> o; }
                return mv;

-- overwrites opt and stort the optimal value
-- boards is rearranged after operation
nextMoveDepth :: Board -> IORef [((Int, Int), Board)] -> Color -> Heuristics -> IORef (Maybe (Mv, Int)) -> Int -> IO ()
nextMoveDepth board boardsRef color mode opt depth = do
       curopt <- readIORef opt
       boards <- readIORef boardsRef
       case curopt of
         Nothing -> return ()
         Just (_, curoptval) -> do
           when (curoptval == winValue) $ throwIO (StopSearch $ printf "Path to the victory was detected. (current depth = %d)" depth)
           when (curoptval == loseValue) $ throwIO (StopSearch $ printf "Path to the defeat was detected. (current depth = %d)" depth)
       vals <- forM boards $ \(mv, bd) -> do
         valPath <- alphaBeta mode bd depth color (oppositeColor color) minValue maxValue
         return (mv, valPath)
       let valsSort = sortBy (flip (compare `on` (fst . snd))) vals
       print valsSort -- sort by value, largest first
       putStrLn ""
       writeIORef boardsRef $ map (\(mv, _) -> (mv, fromMaybe (error "(>_<)") $ lookup mv boards)) valsSort
       let ((i, j), (optval, path)) = if null vals then undefined else head valsSort
       printf "depth = %d, move = (%d, %d), value = %d\n" depth i j optval
       putStrLn $ "path = " ++ show (fmap (M i j :) path)
       writeIORef opt $ Just (M i j, optval)

alphaBeta :: Heuristics -> Board -> Int -> Color -> Color -> Int -> Int -> IO (Int, Maybe [Mv])
alphaBeta mode board depth mycol curcol alpha beta = do
  isGameEnd <- gameEnd board
  if isGameEnd || depth == 0 then do
     result <- staticEval board mycol mode
     return (result, Just [])
  else if curcol == mycol then do
    aref <- newIORef (alpha, Nothing)
    ms <- validMoves board curcol
    let moves = if null ms then [Pass] else map (\(i,j) -> M i j) ms
    forM_ moves $ \m -> do
      (calpha, _) <- readIORef aref
      if calpha >= beta then do
         writeIORef aref (beta, Nothing)
      else do 
        nxt <- doMoveCopy board m curcol
        let nextp = oppositeColor curcol
        (result, path) <- alphaBeta mode nxt (depth - 1) mycol nextp calpha beta
        modifyIORef aref (maxBy (compare `on` fst) (result, fmap (m :) path))
    readIORef aref
  else do
    bref <- newIORef (beta, Nothing)
    ms <- validMoves board curcol
    let moves = if null ms then [Pass] else map (\(i,j) -> M i j) ms
    forM_ moves $ \m -> do
      (cbeta, _) <- readIORef bref
      if alpha >= cbeta then do
         writeIORef bref (alpha, Nothing)
      else do 
        nxt <- doMoveCopy board m curcol
        let nextp = oppositeColor curcol
        (result, path) <- alphaBeta mode nxt (depth - 1) mycol nextp alpha cbeta
        modifyIORef bref (minBy (compare `on` fst) (result, fmap (m :) path))
    readIORef bref

staticEval :: Board -> Color -> Heuristics -> IO Int
staticEval board color mode = do
  isGameEnd <- gameEnd board
  if isGameEnd then do
    my <- count board color 
    opp <- count board (oppositeColor color)
    return $ case compare my opp of
      LT -> loseValue
      EQ -> drawValue
      GT ->  winValue
  else
    ([eval1, eval2, eval3, eval4, eval5] !! (mode - 1)) board color

{- The number of disks -}
eval1 :: Board -> Color -> IO Int
eval1 board color = do
    my <- count board color 
    opp <- count board (oppositeColor color)
    return $ my - opp

eval2 :: Board -> Color -> IO Int
eval2 board color = do
  let opp = oppositeColor color
  ms <- validMoves board opp
  return $ - (length ms)

eval3 :: Board -> Color -> IO Int
eval3 board color = do
  let opp = oppositeColor color
  blc <- count board black
  whc <- count board black
  msmy <- validMoves board color
  msopp <- validMoves board opp
  iv <- count board color
  return $ if blc + whc >= 50 then iv - (blc + whc) else length msmy - length msopp

eval4 :: Board -> Color -> IO Int
eval4 board color = do
  let opp = oppositeColor color
  blc <- count board black
  whc <- count board black
  msmy <- validMoves board color
  msopp <- validMoves board opp
  iv <- count board color
  return $ if blc + whc >= 50 then iv - (blc + whc) else sum (map weightOfPlay msmy) - sum (map weightOfPlay msopp)

eval5 :: Board -> Color -> IO Int
eval5 board color = do
  let opp = oppositeColor color
  blc <- count board black
  whc <- count board black
  msmy <- validMoves board color
  msopp <- validMoves board opp
  iv <- count board color
  myWpop <- weightPop board color
  oppWpop <- weightPop board (oppositeColor color)
  return $ myWpop - oppWpop + sum (map weightOfPlay msmy) - sum (map weightOfPlay msopp)

