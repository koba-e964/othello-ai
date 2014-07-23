module AI where



import Data.Array.IO
import Data.Array.MArray
import Data.List ((\\))
import Data.Maybe
import Control.Monad 

import System.Random
import System.Timeout
import Data.Function
import Data.List
import Data.IORef

import Color 
import Command
import Play

gameEnd :: Board -> IO Bool
gameEnd board = do
  ms1 <- validMoves board black
  ms2 <- validMoves board white
  return $ null ms1 && null ms2

weightOfPlay :: (Int, Int) -> Int
weightOfPlay (i, j) = sub (if i <= 4 then i else 9-i) (if j <= 4 then j else 9-j) where
  sub 1 1 = 10
  sub 1 2 = 2
  sub 2 1 = 2
  sub 2 2 = 1
  sub 1 3 = 4
  sub 3 1 = 4
  sub _ _ = 5
  

myPlay :: Board -> Color -> IO Mv 
myPlay board color =
    do ms <- validMoves board color 
       case ms of 
         [] -> return Pass
         _  -> 
             do
                boards <- mapM (\mv@(mvi, mvj) -> fmap (\x ->(mv,x)) (doMoveCopy board (M mvi mvj) color)) ms
                blc <- count board black
                whc <- count board black
                let depth = if blc + whc >= 54 then 10 else 3
                vals <- fmap catMaybes $ forM boards $ \(mv, bd) -> do
                  val <- timeout 500000 $ alphaBeta bd depth color (oppositeColor color) (-100000000) 100000000 -- timeout 1.0sec
                  case val of {Just v -> return $ Just (mv, v); Nothing -> return Nothing;}
                print vals
                putStrLn ""
                let ((i,j), _optval) = if null vals then (head ms, 0) else maximumBy (compare `on` snd) vals
                return $ M i j 

alphaBeta :: Board -> Int -> Color -> Color -> Int -> Int -> IO Int
alphaBeta board depth mycol curcol alpha beta = do
  isGameEnd <- gameEnd board
  if isGameEnd || depth == 0 then
     staticEval board mycol
  else if curcol == mycol then do
    aref <- newIORef alpha
    ms <- validMoves board curcol
    let moves = if null ms then [Pass] else map (\(i,j) -> M i j) ms
    forM moves $ \m -> do
      calpha <- readIORef aref
      if calpha >= beta then do
         writeIORef aref beta
      else do 
        nxt <- doMoveCopy board m curcol
        let nextp = case m of { Pass -> curcol; _ -> oppositeColor curcol;}
        result <- alphaBeta nxt (depth - 1) mycol nextp calpha beta
        modifyIORef aref (max result)
    readIORef aref
  else do
    bref <- newIORef beta
    ms <- validMoves board curcol
    let moves = if null ms then [Pass] else map (\(i,j) -> M i j) ms
    forM moves $ \m -> do
      cbeta <- readIORef bref
      if alpha >= cbeta then do
         writeIORef bref alpha
      else do 
        nxt <- doMoveCopy board m curcol
        let nextp = case m of { Pass -> curcol; _ -> oppositeColor curcol;}
        result <- alphaBeta nxt (depth - 1) mycol nextp alpha cbeta
        modifyIORef bref (min result)
    readIORef bref

staticEval :: Board -> Color -> IO Int
staticEval board color = do
  isGameEnd <- gameEnd board
  if isGameEnd then do
    my <- count board color 
    opp <- count board (oppositeColor color)
    return $ case compare my opp of
      LT -> -10000000 
      EQ -> - 5000000
      GT ->  10000000
  else
    eval4 board color

{- The number of disks -}
eval1 :: Board -> Color -> IO Int
eval1 = count

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

