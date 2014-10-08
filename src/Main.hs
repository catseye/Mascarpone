module Main where

import System.Environment
import Mascarpone

main = do
    [inputFilename] <- getArgs
    inputProgram <- readFile inputFilename
    return (mascarpone inputProgram)
