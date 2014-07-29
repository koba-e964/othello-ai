module Board where 

import Control.Monad
import Data.Array.IO
import Data.List ((\\))

import Color 
import Command
import Common



{-   functions for Board -}



initBoard :: IO Board 
initBoard = 
    do board <- newArray ( (0,0), (9,9) ) none
       mapM_ (\i ->
                  do writeArray board (i,0) sentinel 
                     writeArray board (i,9) sentinel
                     writeArray board (0,i) sentinel 
                     writeArray board (9,i) sentinel) [0..9]
       writeArray board (4,4) white 
       writeArray board (4,5) black 
       writeArray board (5,4) black 
       writeArray board (5,5) white 
       return board 

isValidMove :: Board -> Color -> (Int,Int) -> IO Bool 
isValidMove board color (i,j) =
    do e <- readArray board (i,j) 
       if e == none then 
           isEffective board color (i,j)
       else
           return False 
-- 8方向
dirs :: [(Int,Int)]
dirs = [ (i,j) | i <- [1,0,-1], j <- [1,0,-1] ] \\ [(0,0)]

-- 石を置いたときに、ひっくり返せるかどうか
isEffective :: Board -> Color -> (Int,Int) -> IO Bool
isEffective board color (i,j) = 
    do ms <- flippableIndices board color (i,j)
       return $ not $ null ms

-- 石を置いたときにひっくり返えるところ
flippableIndices :: Board -> Color -> (Int,Int) -> IO [(Int,Int)]
flippableIndices board color (i,j) = 
    do bs <- mapM (\(di,dj) -> flippableIndicesLine board color (di,dj) (i+di,j+dj)) dirs
       return $ concat bs 

flippableIndicesLine :: Board -> Color -> (Int,Int) -> (Int,Int) -> IO [(Int,Int)]
flippableIndicesLine board color (gdi,gdj) (gi,gj) =
    checkLine (gdi,gdj) (gi,gj) []
    where                    
      ocolor = oppositeColor color
      checkLine (di,dj) (i,j) r =
          do c <- readArray board (i,j) 
             if c == ocolor then 
                 checkLine' (di,dj) (i+di,j+dj) ( (i,j) : r )
             else 
                 return []
      checkLine' (di,dj) (ii,jj) r =
          do c <- readArray board (ii,jj) 
             if c == ocolor then 
                 checkLine' (di,dj) (ii+di,jj+dj) ( (ii,jj) : r )
             else if c == color then 
                      return r 
                  else
                      return []

{- 
   boardをそのまま返すのは一般には危険。
   だが、今のdoMoveの使用方法ならば問題ない。
-}
doMove :: Board -> Mv -> Color -> IO Board 
doMove board GiveUp  _color = return board
doMove board Pass    _color = return board
doMove board (M i j) color =       
    do ms <- flippableIndices board color (i,j)
       mapM_ (\(ii,jj) -> writeArray board (ii,jj) color) ms 
       writeArray board (i,j) color 
       return board 
  

doMoveCopy :: Board -> Mv -> Color -> IO Board
doMoveCopy board move color = do
  copied <- mapArray id board
  doMove copied move color

-- 合法手
validMoves :: Board -> Color -> IO [ (Int,Int) ]
validMoves board color =
     filterM (isValidMove board color) 
             [ (i,j) | i <- [1..8], j <- [1..8]]

-- 石の数
count :: Board -> Color -> IO Int 
count board color =
    do is <- filterM (\i -> 
                          do e <- readArray board i 
                             return $ e == color) 
                     [ (i,j) | i <- [1..8], j <- [1..8]]
       return $ length is


-- 盤面の出力
putBoard :: Board -> IO ()
putBoard board = 
    do putStrLn " |A B C D E F G H " 
       putStrLn "-+----------------"
       mapM_ putBoardLine [1..8]
       putStrLn "  (X: Black,  O: White)"
    where
      putC c | c == none  = putStr " " 
             | c == white = putStr "O"
             | c == black = putStr "X"
             | otherwise  = undefined
      putBoardLine j =
          do putStr $ show j ++ "|"
             mapM_ (\i -> do e <- readArray board (i,j) 
                             putC e >> putStr " ") [1..8]
             putStrLn ""


