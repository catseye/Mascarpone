module Main where

import System.Environment
import Mascarpone

showState (State s _ _) = (show s)

main = do
    args <- getArgs
    case args of
        [fileName] -> do
            c <- readFile fileName
            mascarpone c
            return ()
        ["-d", fileName] -> do
            c <- readFile fileName
            debug c
            return ()
        ["-r", fileName] -> do
            c <- readFile fileName
            r <- mascarpone c
            putStrLn (showState r)
        _ -> do
            putStrLn "Usage: mascarpone [-d|-r] <filename.mascarpone>"
