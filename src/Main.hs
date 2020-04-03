module Main where

import System.Environment
import Language.Mascarpone (mascarpone, debug, getStack)

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
            putStrLn $ show $ getStack r
        _ -> do
            putStrLn "Usage: mascarpone [-d|-r] <filename.mascarpone>"
