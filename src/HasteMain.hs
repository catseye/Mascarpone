{-# LANGUAGE OverloadedStrings #-}

module Main where

import Haste.DOM (withElems, getValue, setProp)
import Haste.Events (onEvent, MouseEvent(Click))
import Haste.Foreign (ffi)

import Language.Mascarpone (getStack, mascarponeWithIO)


getCh :: IO Char
getCh = ffi "(function() {var i=document.getElementById('prog-input'); var s=i.value; i.value=s.substring(1); return s.charCodeAt(0);})"

putCh :: Char -> IO ()
putCh = ffi "(function(c) {var o=document.getElementById('prog-output'); o.textContent += String.fromCharCode(c);})"

clearOutput :: IO ()
clearOutput = ffi "(function(c) {var o=document.getElementById('prog-output'); o.textContent = '';})"

main = withElems ["prog", "result", "run-button"] driver

driver [progElem, resultElem, runButtonElem] =
    onEvent runButtonElem Click $ \_ -> do
        Just prog <- getValue progElem
        clearOutput
        r <- mascarponeWithIO (getCh) (putCh) prog
        setProp resultElem "textContent" $ show $ getStack $ r
