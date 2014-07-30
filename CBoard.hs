module CBoard where

import Common
import Control.Monad
import Data.Bits
import Data.List
import Data.Array.IO

import Color 
import Command

(<<<) :: Places -> Int -> Places
(<<<) = shiftL
infixl 8 <<<

(|||) :: Places -> Places -> Places
(|||) = (.|.)
infixl 5 |||

(&&&) :: Places -> Places -> Places
(&&&) = (.&.)
infixl 7 &&&

{- utility functions for Places -}
placesToPositions :: Places -> [(Int,Int)]
placesToPositions pl =
  map (\x -> (x `mod` 8 + 1, x `div` 8 + 1)) $ sub pl where
  sub 0 = []
  sub x = 
      popCount (x &&& (-x) - 1) : sub (x &&& (x-1))
positionsToPlaces :: [(Int,Int)] -> Places
positionsToPlaces pl =
  foldr (|||) 0 $ map ( \(i,j) -> 1 <<< (i + 8 * j - 9)) pl 

maskLeft :: Places -> Int -> Places -> Places
maskLeft mask l x = (x &&& mask) `shiftL` l

maskRight :: Places -> Int -> Places -> Places
maskRight mask l x = (x &&& mask) `shiftR` l
{- functions for CBoard -}

-- | (4,4) (27) and (5,5) (36) are white
-- | (4,5) (28) and (5,4) (35) are black
initCBoard :: CBoard
initCBoard = CBoard (1 <<< 27 ||| 1 <<< 36) (1 <<< 28 ||| 1 <<< 35)

readCBoard :: CBoard -> Int -> Int -> Color
readCBoard board i j
  | i <= 0 || i >= 9 || j <= 0 || j >= 9 = sentinel
  | otherwise                            = readCBoardUnsafe board i j

readCBoardUnsafe :: CBoard -> Int -> Int -> Color
readCBoardUnsafe (CBoard bl wh) i j =
  let ind = 8 * j + i - 9
      mask= 1 <<< ind :: Places
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
-- 8方向
dirs :: [(Int,Int)]
dirs = [ (i,j) | i <- [1,0,-1], j <- [1,0,-1] ] \\ [(0,0)]

isEffectiveC :: CBoard -> Color -> (Int,Int) -> Bool
isEffectiveC board color (i,j) = 
    let ms = flippableIndicesC board color (i,j) in
    not $ null ms

flippableIndicesC :: CBoard -> Color -> (Int,Int) -> [(Int,Int)]
flippableIndicesC board color (i,j) = 
    concatMap (\(di,dj) -> flippableIndicesLineC board color (di,dj) (i+di,j+dj)) dirs

flippableIndicesLineC :: CBoard -> Color -> (Int,Int) -> (Int,Int) -> [(Int,Int)]
flippableIndicesLineC board color posDiff pos =
    checkLine posDiff pos []
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
doMoveC board GiveUp  _color = board
doMoveC board Pass    _color = board
doMoveC board@(CBoard bl wh) (M i j) color =       
    let ms = (i,j) : flippableIndicesC board color (i,j)
        val = positionsToPlaces ms 
      in
       if color == black
         then CBoard (bl .|. val) (wh .&. complement val)
         else CBoard (bl .&. complement val) (wh .|. val)

-- valid moves (CBoard)
validMovesC :: CBoard -> Color ->  [ (Int,Int) ]
validMovesC board@(CBoard bl wh) color =
  let vacant = complement (bl ||| wh) in
  filter (isEffectiveC board color) $ placesToPositions vacant
--     filter (isValidMoveC board color) 
--             [ (i,j) | i <- [1..8], j <- [1..8]]

-- | set of valid moves represented by Places
validMovesSet :: CBoard -> Color -> Places
validMovesSet (CBoard bl wh) color =
  -- let vacant = complement (bl ||| wh) in
  -- positionsToPlaces $ filter (isEffectiveC board color) $ placesToPositions vacant
  if color == black then
  foldl1 (|||) $ map (\x -> reversibleSetInDir x bl wh) transfers
   else
  foldl1 (|||) $ map (\x -> reversibleSetInDir x wh bl) transfers

transfers :: [Places -> Places]
transfers = [leftTransfer, leftupTransfer, leftdownTransfer, upTransfer, downTransfer, rightupTransfer, rightdownTransfer, rightTransfer]

leftTransfer :: Places -> Places
leftupTransfer :: Places -> Places
leftdownTransfer :: Places -> Places
upTransfer :: Places -> Places
downTransfer :: Places -> Places
rightupTransfer :: Places -> Places
rightdownTransfer :: Places -> Places
rightTransfer :: Places -> Places

leftTransfer      = maskRight 0xffffffffffffff00 8
leftupTransfer    = maskRight 0xfefefefefefefe00 9
leftdownTransfer  = maskRight 0x7f7f7f7f7f7f7f00 7
upTransfer        = maskRight 0xfefefefefefefefe 1
downTransfer      = maskLeft  0x7f7f7f7f7f7f7f7f 1
rightupTransfer   = maskLeft  0x00fefefefefefefe 7
rightdownTransfer = maskLeft  0x007f7f7f7f7f7f7f 9
rightTransfer     = maskLeft  0x00ffffffffffffff 8

reversibleSetInDir :: (Places -> Places) -> Places -> Places -> Places
reversibleSetInDir trans my opp = let
  vacant = complement (my ||| opp)
  opps = scanl1 (&&&) $ take 6 $ iterate trans (trans opp)
  mys  = take 6 $ iterate trans (trans $ trans my) in
  vacant &&& foldl1 (|||) (zipWith (&&&) opps mys)

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
             | otherwise  = undefined
      putBoardLine j =
          show j ++ "|" ++
           concatMap (\i -> let e = readCBoardUnsafe board i j in 
                             putC e ++ " ") [1..8]
             ++ "\n"

{- convertion between CBoard and Board -}

cboardToBoard :: CBoard -> IO Board
boardToCBoard :: Board -> IO CBoard
writeToBoard :: CBoard -> Board -> IO ()

cboardToBoard board = do
  ary <- newArray ((0,0), (9,9)) none
  writeToBoard board ary
  return ary


boardToCBoard board = do
    let trans = map $ \(i,j) -> 1 <<< (i + 8 * j - 9) :: Places
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

