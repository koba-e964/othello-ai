{-# LANGUAGE BangPatterns #-}

module Main where

import System.Environment (getArgs) 
import Network 

import Command
import AI
import Common
import CBoard

import Text.Printf 
import System.IO
import Data.IORef

import Color 
import Control.Monad
import System.Console.GetOpt

data Config 
    = Config { host :: HostName,
               port :: PortNumber, 
               playerName :: String,
               verbose :: Bool,
               helpMode :: Bool,
               heuristicsMode :: Heuristics,
               thinkingTime ::Int, -- in microseconds
               losing :: Bool -- attempt to lose
               } 

type WinLose = IORef (Int, Int)
addWin :: WinLose -> IO ()
addWin arg = do
  (x,y) <- readIORef arg
  writeIORef arg (x+1,y)

addLose :: WinLose -> IO ()
addLose arg = do
  (x,y) <- readIORef arg
  writeIORef arg (x,y+1)

showWinLose :: WinLose -> IO String
showWinLose arg = do
  (x,y) <- readIORef arg
  return $ "(Win, Lose) = (" ++ show x ++ ", " ++ show y ++ ")"

defaultConf :: Config
defaultConf = Config "localhost" 3000 "KobaAI" False False 5 500000 False -- default name is changed.

options :: [OptDescr (Config -> Config)]
options =
    [ Option ['v'] ["verbose"]
             (NoArg $ \conf -> conf { verbose = True })
             "verbose mode"
    , Option ['H'] ["host"]
             (ReqArg (\s conf -> conf { host = s }) "HOST")
             "host name of a server" 
    , Option ['p'] ["port"]
             (ReqArg (\s conf -> conf { port = fromIntegral (read s :: Int) }) "PORT")
             "port number of a server"
    , Option ['n'] ["name"]
             (ReqArg (\s conf -> conf { playerName = s }) "NAME")
             "player name" 
    , Option ['h','?'] ["help"]
             (NoArg (\conf -> conf { helpMode = True }))
             "show this help"
    , Option ['s'] ["heuristics"]
             (ReqArg (\s conf -> conf { heuristicsMode = read s :: Heuristics }) "HEURISTICS")
             "the kind of routine (1..5)"
    , Option ['t'] ["time"]
             (ReqArg (\s conf -> conf { thinkingTime = let t = read s :: Double in if t < 0 || t >= 6e5 then 500000 else round (t * 1e3) }) "THINKTIME")
             "the length of time to think (in millisecond (ms)) (max: 600000ms (10min.)"
    , Option ['l'] ["losing"]
             (NoArg $ \conf -> conf { losing = True })
             "attempt to lose"
    ]

usageMessage :: String
usageMessage = usageInfo header options 
    where
      header =
          "Usage: \n" 
          ++ "    reversi -H HOST -p PORT -n NAME ...\n" 

parseArg :: [String] -> IO (Config, [String]) 
parseArg args =
    case getOpt Permute options args of 
      (o, n, [])  -> return (foldl (flip ($)) defaultConf o, n)
      (_, _, err) -> ioError (userError (concat err ++ usageMessage))
      
hGetCommand :: Handle -> IO Command 
hGetCommand h = 
    do { r <- hGetLine h 
       ; putStrLn $ "Received: " ++ r 
       ; return $ either error id $ parseCommand r 
       }

hGetCommand' :: Handle -> IO Command
hGetCommand' h = 
    do { c <- hGetCommand h  
       ; case c of 
           Empty -> hGetCommand' h  
           _     -> return c }


    
hPutCommand :: Show a => Handle -> a -> IO () 
hPutCommand h c = 
    do hPutStr h (show c)
       hPutStr h "\n"
       hFlush h 
       putStrLn $ "Sent: " ++ show c 

-- History                         
data OPMove = OMove Mv | PMove Mv 

instance Show OPMove where 
    show (OMove mv) = "-" ++ show mv
    show (PMove mv) = "+" ++ show mv 
type Hist = [OPMove]

showHist :: [OPMove] -> String 
showHist hist = 
    foldr (\a r -> show a ++ " " ++ r) "" $ reverse hist 
    

showScores :: [(String, (Int, Int, Int))] -> String 
showScores scores =
    foldr (\(n,(s,w,l)) r -> 
               take (len+1) (n ++ ":" ++ cycle " ") 
                    ++ replicate (1 + slen - length (show s)) ' ' ++ show s 
                    ++ " (Win " ++ show w ++ ", Lose " ++ show l ++ ")\n" ++ r) "" scores 
    where
      len  = maximum $ map (length . fst) scores
      slen = maximum $ map (length . show . (\(_,(a,_,_)) -> a)) scores 

-- クライアント
client :: Config -> IO () 
client !conf =
    do { putStrLn $ "Connecting to " ++ host conf ++ " " ++ show (port conf)
       ; h <- connectTo (host conf) (PortNumber $ port conf) 
       ; putStrLn "Connection Ok."
       ; hPutCommand h $ Open (playerName conf)
       ; winLose <- newIORef (0,0)
       ; waitStart h conf winLose }

waitStart :: Handle -> Config -> WinLose -> IO () 
waitStart !h !conf winLose =
    do { c <- hGetCommand' h 
       ; case c of 
           Bye scores -> 
               putStr $ showScores scores 
           Start color opname mytime ->
               do { board <- newIORef initCBoard 
                  ; if color == black then 
                        performMyMove h board color [] opname mytime conf winLose
                    else
                        waitOpponentMove h board color [] opname mytime conf winLose}
           _ -> 
               error $ "Invalid Command: " ++ show c 
       }

performMyMove :: Handle -> IORef CBoard -> Color -> [OPMove] -> String -> Int -> Config -> WinLose -> IO ()
performMyMove h boardRef color hist opname _mytime conf winLose =
    do { cboard <- readIORef boardRef
       ; pmove <- myPlay cboard color (heuristicsMode conf) (thinkingTime conf) (losing conf)
       ; let newCBoard = doMoveC cboard pmove color
       ; writeIORef boardRef newCBoard
       ; hPutCommand h $ Move pmove 
       ; when (verbose conf) $ putStrLn $ replicate 80 '-'
       ; when (verbose conf) $ putStrLn ("PMove: " ++ show pmove ++ " " ++ showColor color) 
       ; when (verbose conf) (putStr $ showCBoard newCBoard)
       ; c <- hGetCommand' h 
       ; case c of 
           Ack mytime' -> 
               waitOpponentMove h boardRef color (PMove pmove:hist) opname mytime' conf winLose
           End wl n m r ->
               procEnd h boardRef color hist opname wl n m r conf winLose
           _ -> 
               error $ "Invalid Command: " ++ show c 
       }

waitOpponentMove :: Handle -> IORef CBoard -> Color -> [OPMove] -> String -> Int -> Config -> WinLose -> IO ()
waitOpponentMove h boardRef color hist opname mytime conf winLose =
    do { c <- hGetCommand' h
       ; case c of 
           Move omove ->
               do { cboard <- readIORef boardRef
                  ; let newBoard = doMoveC cboard omove (oppositeColor color)
                  ; writeIORef boardRef newBoard
                  ; when (verbose conf) $ putStrLn $ replicate 80 '-'
                  ; when (verbose conf) $ putStrLn ("OMove: " ++ show omove ++ " " ++ showColor (oppositeColor color))
                  ; when (verbose conf) (putStr $ showCBoard newBoard)
                  ; performMyMove h boardRef color (OMove omove:hist) opname mytime conf winLose }
           End wl n m r -> 
               procEnd h boardRef color hist opname wl n m r conf winLose
           _ -> 
               error $ "Invalid Command: " ++ show c 
       }
           
procEnd :: Handle -> IORef CBoard -> Color -> [OPMove] -> String -> WL -> Int -> Int -> String -> Config -> WinLose -> IO ()
procEnd h boardRef color hist opname wl n m r conf winLose =
    do { case wl of 
           Win -> do
               putStrLn $ printf "You win! (%d vs. %d) -- %s." n m r
               addWin winLose
           Lose -> do
               putStrLn $ printf "You lose! (%d vs. %d) -- %s." n m r
               addLose winLose
           Tie ->
               putStrLn $ printf "Draw (%d vs. %d) -- %s." n m r
       ; putStrLn $ printf "Your name: %s (%s)  Oppnent name: %s (%s)."
                      (playerName conf) (showColor color)
                      opname (showColor (oppositeColor color))
       ; cboard <- readIORef boardRef
       ; putStr $ showCBoard cboard
       ; putStrLn $ showHist hist 
       ; putStrLn =<< showWinLose winLose
       ; waitStart h conf winLose }
                  
main :: IO () 
main = withSocketsDo $ 
       do { args <- getArgs 
          ; (conf, _rest) <- parseArg args 
          ; if helpMode conf then 
                putStrLn usageMessage 
            else 
                client conf }
          
