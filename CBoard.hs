{-# LANGUAGE BangPatterns #-}
module CBoard where

import Common
import Data.Bits
import Data.List

import Color 
import Command

(<<<) :: Places -> Int -> Places
(<<<) = shiftL
infixl 8 <<<

(>>>) :: Places -> Int -> Places
(>>>) = shiftR
infixl 8 >>>

(|||) :: Places -> Places -> Places
(|||) = (.|.)
infixl 5 |||

(&&&) :: Places -> Places -> Places
(&&&) = (.&.)
infixl 7 &&&

{- utility functions for Places -}
placesToPositions :: Places -> [(Int,Int)]
placesToPositions !pl =
  map (\x -> (x `mod` 8 + 1, x `div` 8 + 1)) $ sub pl where
  sub 0 = []
  sub x = 
      popCount (x &&& (-x) - 1) : sub (x &&& (x-1))
positionsToPlaces :: [(Int,Int)] -> Places
positionsToPlaces !pl =
  foldr (|||) 0 $ map ( \(i,j) -> 1 <<< (i + 8 * j - 9)) pl 

maskLeft :: Places -> Int -> Places -> Places
maskLeft !mask !l !x = (x &&& mask) `shiftL` l

maskRight :: Places -> Int -> Places -> Places
maskRight !mask !l !x = (x &&& mask) `shiftR` l

-- | Converts a set to a list of places.
-- | Every element in returned list is a power of 2.
setToDisks :: Places -> [Places]
setToDisks 0 = []
setToDisks !set = 
      (set &&& (-set)) : setToDisks (set &&& (set-1))

{- functions for CBoard -}

-- | (4,4) (27) and (5,5) (36) are white
-- | (4,5) (28) and (5,4) (35) are black
initCBoard :: CBoard
initCBoard = CBoard (1 <<< 28 ||| 1 <<< 35) (1 <<< 27 ||| 1 <<< 36)

readCBoard :: CBoard -> Int -> Int -> Color
readCBoard !board !i !j
  | i <= 0 || i >= 9 || j <= 0 || j >= 9 = sentinel
  | otherwise                            = readCBoardUnsafe board i j

readCBoardUnsafe :: CBoard -> Int -> Int -> Color
readCBoardUnsafe (CBoard !bl !wh) !i !j =
  let ind = 8 * j + i - 9
      mask= 1 <<< ind :: Places
      bbit = bl .&. mask
      wbit = wh .&. mask
    in
     if bbit /= 0 then black else if wbit /= 0 then white else none
  
isValidMoveC :: CBoard -> Color -> (Int,Int) -> Bool 
isValidMoveC !board !color (!i,!j) =
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
doMoveC (CBoard bl wh) (M i j) color =       
{-    let ms = (i,j) : flippableIndicesC board color (i,j)
        val = positionsToPlaces ms 
      in
-}
      let !disk = 1 <<< (i + 8 * j - 9) in
          if color == black then
            let (!nbl, !nwh) = doMoveBit bl wh disk in
            CBoard nbl nwh
           else
            let (!nwh, !nbl) = doMoveBit wh bl disk in
            CBoard nbl nwh

-- | disk must be a singleton.
doMoveBit :: Places -> Places -> Places -> (Places, Places)
doMoveBit !my !opp !disk = 
   let !val = flippableIndicesSet my opp disk in
   (my ||| val ||| disk, opp &&& complement val)
  
-- valid moves (CBoard)
validMovesC :: CBoard -> Color ->  [ (Int,Int) ]
validMovesC board@(CBoard !bl !wh) !color =
  let vacant = complement (bl ||| wh) in
  filter (isEffectiveC board color) $ placesToPositions vacant
--     filter (isValidMoveC board color) 
--             [ (i,j) | i <- [1..8], j <- [1..8]]

-- | set of valid moves represented by Places
validMovesSet :: CBoard -> Color -> Places
validMovesSet (CBoard !bl !wh) !color =
  -- let vacant = complement (bl ||| wh) in
  -- positionsToPlaces $ filter (isEffectiveC board color) $ placesToPositions vacant
  if color == black then
  validMovesSetMO bl wh
   else
  validMovesSetMO wh bl

-- | set of valid moves represented by Places
-- | reference : http://code.google.com/p/edax-reversi/source/browse/src/board.c
validMovesSetMO :: Places -> Places -> Places
validMovesSetMO !bl !wh =
  let !mask = wh &&& 0x7e7e7e7e7e7e7e7e :: Places
      !r1 = vmSubMO bl mask 1
      !r2 = vmSubMO bl wh 8
      !r3 = vmSubMO bl mask 7
      !r4 = vmSubMO bl mask 9
    in
     (r1 ||| r2 ||| r3 ||| r4) &&& complement (bl ||| wh)

vmSubMO :: Places -> Places -> Int -> Places
vmSubMO !my !mask !dir = let
  !dir2 = dir + dir
  !fl1 = mask &&& (my <<< dir)
  !fr1 = mask &&& (my >>> dir)
  !fl2 = fl1 ||| mask &&& (fl1 <<< dir)
  !fr2 = fr1 ||| mask &&& (fr1 >>> dir)
  !maskl = mask &&& (mask <<< dir)
  !maskr = mask &&& (mask >>> dir)
  !fl3 = fl2 ||| maskl &&& (fl2 <<< dir2)
  !fr3 = fr2 ||| maskr &&& (fr2 >>> dir2)
  !fl4 = fl3 ||| maskl &&& (fl3 <<< dir2)
  !fr4 = fr3 ||| maskr &&& (fr3 >>> dir2) in
   fl4 <<< dir ||| fr4 >>> dir

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

-- | disk must be a singleton
flippableIndicesSet :: Places -> Places -> Places -> Places
flippableIndicesSet !my !opp !disk =
  foldl1' (|||) (map (\x -> flippableIndicesInDir x my opp disk) transfers) 

-- reference: http://ja.wikipedia.org/wiki/%E3%82%AA%E3%82%BB%E3%83%AD%E3%81%AB%E3%81%8A%E3%81%91%E3%82%8B%E3%83%93%E3%83%83%E3%83%88%E3%83%9C%E3%83%BC%E3%83%89
flippableIndicesInDir :: (Places -> Places) -> Places -> Places -> Places -> Places
flippableIndicesInDir !trans !my !opp !disk = let
  !ma = trans disk
  (!rev, !mask) = sub 0 ma in
  if mask &&& my /= 0 then rev else 0 where
    sub !rev 0 = (rev, 0)
    sub !rev !msk
       | msk &&& opp == 0 = (rev, msk)
       | otherwise         = sub (rev ||| msk) (trans msk)

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

