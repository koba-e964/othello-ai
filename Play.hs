module Play where 

import Control.Monad
import Control.Monad.ST
import Data.Bits
import Data.Int
import Data.Array.IO
import Data.Array.MArray
import Data.List ((\\), foldl')
import Data.STRef

import System.Random
import Text.Printf

import Color 
import Command


-- 番兵付きの10x10配列
type Board = IOUArray (Int,Int) Int 

-- | A efficient data structure for states of reversi board.
-- | each bit corresponds to a cell in the board:
-- |  0  1  2  3  4  5  6  7
-- |  8  9 10 11 12 13 14 15
-- | ...
-- | 56 57 58 59 60 61 62 63
-- | (i,j) <====> 8*(j-1)+(i-1)
-- | the first Int64 represents the positions of black disks.
-- | the second Int64 represents the positions of white disks.
data CBoard = CBoard !Int64 !Int64


{- functions for CBoard -}

-- | (4,4) (27) and (5,5) (36) are white
-- | (4,5) (28) and (5,4) (35) are black
initCBoard :: CBoard
initCBoard = CBoard (2^27 + 2^36) (2^28 + 2^35)

readCBoard :: CBoard -> Int -> Int -> Color
readCBoard board i j
  | i <= 0 || i >= 9 || j <= 0 || j >= 9 = sentinel
  | otherwise                            = readCBoardUnsafe board i j

readCBoardUnsafe :: CBoard -> Int -> Int -> Color
readCBoardUnsafe board@(CBoard bl wh) i j =
  let ind = 8 * j + i - 9
      mask= 1 `shiftL` ind :: Int64
      bbit = bl .&. mask
      wbit = wh .&. mask
    in
     if bbit /= 0 then black else if wbit /= 0 then white else none
  
isValidMoveC :: CBoard -> Color -> (Int,Int) -> Bool 
isValidMoveC board color (i,j) =
    let e = readCBoard board i j in
       if e == none then 
           isEffectiveC board color (i,j)
       else
           False 

isEffectiveC :: CBoard -> Color -> (Int,Int) -> Bool
isEffectiveC board color (i,j) = 
    let ms = flippableIndicesC board color (i,j) in
    not $ null ms

flippableIndicesC :: CBoard -> Color -> (Int,Int) -> [(Int,Int)]
flippableIndicesC board color (i,j) = 
    concatMap (\(di,dj) -> flippableIndicesLineC board color (di,dj) (i+di,j+dj)) dirs

flippableIndicesLineC :: CBoard -> Color -> (Int,Int) -> (Int,Int) -> [(Int,Int)]
flippableIndicesLineC board color (di,dj) (i,j) =
    checkLine (di,dj) (i,j) []
    where                    
      ocolor = oppositeColor color
      checkLine (di,dj) (i,j) r =
          let c = readCBoard board i j in
             if c == ocolor then 
                 checkLine' (di,dj) (i+di,j+dj) ( (i,j) : r )
             else 
                 []
      checkLine' (di,dj) (i,j) r =
          let c = readCBoard board i j in
             if c == ocolor then 
                 checkLine' (di,dj) (i+di,j+dj) ( (i,j) : r )
             else if c == color then 
                      r 
                  else
                      []

doMoveC :: CBoard -> Mv -> Color -> CBoard 
doMoveC board GiveUp  color = board
doMoveC board Pass    color = board
doMoveC board@(CBoard bl wh) (M i j) color =       
    let ms = (i,j) : flippableIndicesC board color (i,j)
        val = runST $ do
           st <- newSTRef $ 0 :: ST s (STRef s Int64)
           forM_ ms $ \(ii,jj) -> do
             modifySTRef st ((.|.) (1 `shiftL` (ii + 8 * jj - 9)))
           readSTRef st
      in
       if color == black
         then CBoard (bl .|. val) (wh .&. complement val)
         else CBoard (bl .&. complement val) (wh .|. val)

-- valid moves (CBoard)
validMovesC :: CBoard -> Color ->  [ (Int,Int) ]
validMovesC board color =
     filter (isValidMoveC board color) 
             [ (i,j) | i <- [1..8], j <- [1..8]]

countC :: CBoard -> Color -> Int 
countC (CBoard bl wh) color 
  | color == black = popCount bl
  | color == white = popCount wh
  | otherwise      = error $ "invalid color:" ++ show color
showCBoard :: CBoard -> String
showCBoard board = 
    " |A B C D E F G H \n" ++
      "-+----------------\n" ++
       concatMap putBoardLine [1..8] ++
        "  (X: Black,  O: White)"
    where
      putC c | c == none  = " " 
             | c == white = "O"
             | c == black = "X"
      putBoardLine j =
          show j ++ "|" ++
           concatMap (\i -> let e = readCBoardUnsafe board i j in 
                             putC e ++ " ") [1..8]
             ++ "\n"

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

flippableIndicesLine board color (di,dj) (i,j) =
    checkLine (di,dj) (i,j) []
    where                    
      ocolor = oppositeColor color
      checkLine (di,dj) (i,j) r =
          do c <- readArray board (i,j) 
             if c == ocolor then 
                 checkLine' (di,dj) (i+di,j+dj) ( (i,j) : r )
             else 
                 return []
      checkLine' (di,dj) (i,j) r =
          do c <- readArray board (i,j) 
             if c == ocolor then 
                 checkLine' (di,dj) (i+di,j+dj) ( (i,j) : r )
             else if c == color then 
                      return r 
                  else
                      return []

{- 
   boardをそのまま返すのは一般には危険。
   だが、今のdoMoveの使用方法ならば問題ない。
-}
doMove :: Board -> Mv -> Color -> IO Board 
doMove board GiveUp  color = return board
doMove board Pass    color = return board
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

-- とても弱い
-- 置けるところに置くだけのプログラム。
play :: Board -> Color -> IO Mv 
play board color =
    do ms <- validMoves board color 
       case ms of 
         [] -> return Pass
         _  -> 
             do k <- getStdRandom $ randomR (0, length ms-1)
                let (i,j) = ms !! k 
                return $ M i j 

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
      putBoardLine j =
          do putStr $ show j ++ "|"
             mapM_ (\i -> do e <- readArray board (i,j) 
                             putC e >> putStr " ") [1..8]
             putStrLn ""

{- convertion between CBoard and Board -}

cboardToBoard :: CBoard -> IO Board
boardToCBoard :: Board -> IO CBoard
writeToBoard :: CBoard -> Board -> IO ()

cboardToBoard board = do
  ary <- newArray ((0,0), (9,9)) none
  writeToBoard board ary
  return ary


boardToCBoard board = do
    let trans = map $ \(i,j) -> 1 `shiftL` (i + 8 * j - 9) :: Int64
    bls <- fmap trans $ flip filterM [(i,j) | i <- [1..8], j <- [1..8]] $ \(i,j) -> do
        el <- readArray board (i,j)
        return $ el == black
    whs <- fmap trans $ flip filterM [(i,j) | i <- [1..8], j <- [1..8]] $ \(i,j) -> do
        el <- readArray board (i,j)
        return $ el == white
    return $ CBoard (foldl' (.|.) 0 bls) (foldl' (.|.) 0 whs)

writeToBoard cboard board= do
  mapM_ (\i ->
     do writeArray board (i,0) sentinel 
        writeArray board (i,9) sentinel
        writeArray board (0,i) sentinel 
        writeArray board (9,i) sentinel) [0..9]
  forM_ [(i,j) | i <- [1..8], j <- [1..8]] $ \(i,j) ->
    writeArray board (i,j) (readCBoardUnsafe cboard i j)
  return ()

