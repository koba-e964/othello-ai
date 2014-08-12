module Human where

import Common
import Command
import CBoard
import Color
import Data.Bits
import Data.List
import Data.Maybe
import System.IO

readHuman :: CBoard -> Color -> IO Mv

readHuman board@(CBoard bl wh) color = do
    putStrLn $ showCBoard board;
    let bc = popCount bl
        wc = popCount wh
        valid = validMovesC board color
    putStrLn $ "black:" ++ show bc ++ " white:" ++ show wc
    putStrLn $ "You: " ++ showColor color
    if null valid then do
        putStrLn "You have to pass."
        return Pass
    else do
        (i,j) <- readMove valid
        return (M i j);



readMove :: [(Int,Int)] -> IO (Int,Int)
readMove valid = do
    putStr "> "
    hFlush stdout
    line <- getLine :: IO String
    if length line /= 2 then do
        putStrLn "invalid format (input must be of form \"a4\" (quotes for clarity)"
        readMove valid
     else do
        let [col,row] = line
        if not (elem col "abcdefgh") then do
            putStrLn $ "invalid column: " ++ [col]
            readMove valid
         else do
            if not (elem row "12345678") then do
                putStrLn $ "invalid row: " ++ [row]
                readMove valid
             else do
                let (i,j) = (fromMaybe (error "???") (findIndex (== col) "abcdefgh") + 1, fromMaybe (error "???") (findIndex (== row) "12345678") + 1)
                if elem (i,j) valid then
                    return (i,j)
                 else do
                    putStrLn $ "invalid move: " ++ show (M i j)
                    readMove valid


