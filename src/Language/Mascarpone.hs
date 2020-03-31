--
-- Copyright (c)2007-2020 Chris Pressey, Cat's Eye Technologies.
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
--
--   1. Redistributions of source code must retain the above copyright
--      notices, this list of conditions and the following disclaimer.
--   2. Redistributions in binary form must reproduce the above copyright
--      notices, this list of conditions, and the following disclaimer in
--      the documentation and/or other materials provided with the
--      distribution.
--   3. Neither the names of the copyright holders nor the names of their
--      contributors may be used to endorse or promote products derived
--      from this software without specific prior written permission. 
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
-- FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
-- COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
-- INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
-- BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
-- LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
-- ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.
--

--
-- mascarpone.hs v2020.smth
--
-- Reference interpreter for
-- The Mascarpone Programming Language
-- v1.0
--

module Language.Mascarpone where

import qualified Data.Map as Map
import qualified Data.Char as Char

-----------------------------------------------------------------------
-- ============================ Symbols ============================ --
-----------------------------------------------------------------------

type Symbol = Char


-----------------------------------------------------------------------
-- ============================== Data ============================= --
-----------------------------------------------------------------------

data Datum = Symbol Symbol
           | Operation Operation
           | Interpreter Interpreter
    deriving (Show)


-----------------------------------------------------------------------
-- ============================= Stacks ============================ --
-----------------------------------------------------------------------

data Stack = Stack [Datum]

instance Show Stack where
    show (Stack datumList) =
        "[(bottom) " ++ showStack (reverse datumList) ++ "(top)]"

showStack [] = ""
showStack ((Symbol sym):tail) = "'" ++ [sym] ++ "' " ++ (showStack tail)
showStack ((Operation op):tail) = (show op) ++ " " ++ (showStack tail)
showStack ((Interpreter i):tail) = (show i) ++ " " ++ (showStack tail)

pop (Stack (head:tail)) = (head, Stack tail)
push (Stack tail) head  = (Stack (head:tail))

pushString stack string = pushString' stack ("[" ++ string ++ "]")
pushString' stack [] = stack
pushString' stack (head:tail) =
    let
        stack' = push stack (Symbol head)
    in
        pushString' stack' tail

popString (Stack ((Symbol ']'):tail)) = popString' (Stack tail) 0

popString' (Stack ((Symbol ']'):tail)) level =
    let
        (string, stack') = popString' (Stack tail) (level + 1)
    in
        (string ++ [']'], stack')
popString' (Stack ((Symbol '['):tail)) 0 = ([], Stack tail)
popString' (Stack ((Symbol '['):tail)) level =
    let
        (string, stack') = popString' (Stack tail) (level - 1)
    in
        (string ++ ['['], stack')
popString' (Stack ((Symbol head):tail)) level =
    let
        (string, stack') = popString' (Stack tail) level
    in
        (string ++ [head], stack')


-----------------------------------------------------------------------
-- ======================== Program States ========================= --
-----------------------------------------------------------------------

data State = State {
    stack :: Stack,
    interpreter :: Interpreter,
    debugger :: Debugger,
    getCh :: IO Char,
    putCh :: Char -> IO ()
}



getInterpreter State{ interpreter=i } = i
setInterpreter state i = state{ interpreter=i }

getStack State{ stack=s } = s

statePush st@State{ stack=s } head = st{ stack=(push s head) }
statePushString st@State{ stack=s } str = st{ stack=(pushString s str) }

statePop st@State{ stack=s } =
    let
        (elem, s') = pop s
    in
        (elem, st{ stack=s' })
statePopString st@State{ stack=s } =
    let
        (string, s') = popString s
    in
        (string, st{ stack=s' })

stateDebug program st@State{ debugger=debugger } =
    debugger program st


-----------------------------------------------------------------------
-- ========================= Interpreters ========================== --
-----------------------------------------------------------------------

--
-- An interpreter maps symbols onto operations.  The map is given as a
-- finite function (a finite set of pairs of symbols and operations,)
-- plus an operation which is the "default" which is assumed when there
-- is no explicit operation present for a given symbol.  Each interpreter
-- also has a "parent" interpreter, which may be null (NoInterp.)
--

data InterpreterSort = Initial
                     | DeepQuote
                     | SingleQuote
                     | Custom

data Interpreter = Interp InterpreterSort (Map.Map Symbol Operation) Operation Interpreter
                 | NoInterp

instance Show Interpreter where
    show (Interp Initial _ _ parent) =
        "|Initial|->" ++ (show parent)
    show (Interp DeepQuote _ _ parent) =
        "|DeepQuote|->" ++ (show parent)
    show (Interp SingleQuote _ _ parent) =
        "|SingleQuote|->" ++ (show parent)
    show (Interp Custom map def parent) =
        "|" ++ (show map) ++ ", default=" ++ (show def) ++ "|->" ++ (show parent)
    show NoInterp =
        "|None|"

--
-- Retrieve the operation associated with the given symbol.
--

fetch (Interp _ map def _) sym = Map.findWithDefault def sym map

--
-- Return a derived interpreter where the given symbol is associated
-- with the given operation.
--

supplant (Interp _ map def parent) sym op = (Interp Custom (Map.insert sym op map) def parent)

--
-- Retrieve the parent interpreter of the given interpreter.
--

getParent (Interp _ _ _ parent) = parent

--
-- Return a derived interpreter where the parent interpreter is the given
-- interpreter.
--

setParent (Interp sort map def _) parent = Interp sort map def parent


-----------------------------------------------------------------------
-- ========================== Operations =========================== --
-----------------------------------------------------------------------

--
-- An operation is a string of symbols given meaning by an interpreter.
--
-- Of course, that definition is more conceptual than practical;
-- in this implementation, we also have Intrinsic operations, which are
-- part of the inital Mascarpone interpreter, and are defined by
-- Haskell functions.
--
-- The Symbol in the Intrinsic alternative is only for aesthetic
-- purposes: it indicates what symbol is associated with the
-- operation in the initial Mascarpone interpreter, so that Show
-- Operation, and the result of expandOp, are somewhat more human-
-- friendly.  However, it is semantically immaterial.
--

data Operation = Intrinsic Symbol (State -> IO State)
               | Program [Symbol] Interpreter

instance Show Operation where
    show (Intrinsic sym _) = "[[intrinsic '" ++ [sym] ++ "']]"
    show (Program string interpreter) = "[[" ++ show string ++ "/" ++ show interpreter ++ "]]"

--
-- Execute the given operation in the given state.
--

execute :: Operation -> State -> IO State

execute (Intrinsic _ f) state =
    f state

--
-- Note that when we call an operation that was defined using a "captured"
-- interpreter, we do the following:
--
-- 1.  We attach the current interpreter as the parent interpreter of the
--     captured interpreter
-- 2.  We interpret the symbols in the operation definition using the captured
--     interpreter
-- 3.  When we have reached the end, we extract the parent interpreter of the
--     captured interpreter and use it as the new current interpreter
--
-- Note that this means two things:
--
-- 1.  The operation definition may modify its current interpreter (the
--     captured interpreter) to its heart's content; this will not modify
--     our current interpreter (the parent interpreter)
-- 2.  The operation definition may modify our current interpreter by
--     modifying the parent interpreter of its current interpreter.
--

execute (Program programText capturedInterpreter) state =
    let
        callersInterpreter = getInterpreter state
        capturedInterpreter' = setParent capturedInterpreter callersInterpreter
        state' = setInterpreter state capturedInterpreter'
    in
        execute' programText state'

execute' [] state =
    let
        capturedInterpreter = getInterpreter state
        callersInterpreter = getParent capturedInterpreter
        state' = setInterpreter state callersInterpreter
    in
        do return state'

execute' program@(symbol:tail) state =
    let
        interpreter = getInterpreter state
        operation = fetch interpreter symbol
    in do
        stateDebug program state
        state' <- execute operation state
        execute' tail state'

--
-- Expand an operation into a program (string of symbols) and an
-- interpreter, such that the string of symbols, when interpreted
-- by that interpreter, does the same things as the operation.
--
-- This happens to return, for program-defined operations, the same
-- program and interpreter that the operation was created using,
-- and for intrinsic operations, the program consisting only of
-- the symbol used for that intrinsic operation in the inital
-- Mascarpone interpreter, plus the initial Mascarpone interpreter.
-- However, there are an infinite number of other possible correct
-- returns.
--

expandOp :: Operation -> ([Symbol], Interpreter)

expandOp (Program str interp) =
    (str, interp)
expandOp (Intrinsic sym _) =
    let
        prog = [sym]
    in
        (prog, initialInterpreter)


------------------------------------------------------------
--------------- The operations themselves. -----------------
------------------------------------------------------------


--
-- Miscellaneous operations.
--

--
-- Do nothing.
--

opNop state =
    do return state

--
-- Push the null interpreter onto the stack.
--

opPushNullInterpreter state =
    do return (statePush state (Interpreter NoInterp))

--
-- Pop an operation, create an interpreter where all symbols
-- are associated with that operation, and push that interpreter
-- onto the stack.
--

opMakeUniformInterpreter state =
    let
        ((Operation op), state') = statePop state
        interpreter = Interp Custom Map.empty op NoInterp
    in
        do return (statePush state' (Interpreter interpreter))

--
-- Push the current interpreter onto the stack.
--

opReify state =
    let
        interpreter = getInterpreter state
        state' = statePush state (Interpreter interpreter)
    in
        do return state'

--
-- Pop an interpreter from the stack and use it to interpret
-- the remainder of the program.
--

opDiefy state =
    let
        ((Interpreter interpreter), state') = statePop state
        state'' = setInterpreter state' interpreter
    in
        do return state''

--
-- Pop an interpreter from the stack, and push back onto
-- the stack the interpreter's parent interpreter.
--

opGetParent state =
    let
        ((Interpreter interpreter), state') = statePop state
        state'' = statePush state' (Interpreter (getParent interpreter))
    in
        do return state''

--
-- Pop an interpreter i from the stack, then another interpreter j.
-- Push a new interpreter that is the same as i, but has j as its parent.
--
-- So the stack looks like this:  newParent oldInterp -> newInterp
--

opSetParent state =
    let
        ((Interpreter interpreter), state') = statePop state
        ((Interpreter parent), state'') = statePop state'
        interpreter' = setParent interpreter parent
        state''' = statePush state'' (Interpreter interpreter')
    in
        do return state'''

--
-- Pop a symbol and an interpreter and push the operation that
-- corresponds with that symbol in that interpreter.
--

opExtractOp state =
    let
        ((Symbol sym), state') = statePop state
        ((Interpreter interp), state'') = statePop state'
        op = fetch interp sym
        state''' = statePush state'' (Operation op)
    in
        do return state'''

--
-- Pop a symbol, an operation, and an interpreter, and push a new
-- interpreter in which that symbol is associated with that operation.
--

opInstallOp state =
    let
        ((Symbol sym), state') = statePop state
        ((Operation op), state'') = statePop state'
        ((Interpreter interp), state''') = statePop state''
        interp' = supplant interp sym op
        state'''' = statePush state''' (Interpreter interp')
    in
        do return state''''

--
-- Pop an operation from the stack and perform it.
--

opPerform state =
    let
        ((Operation op), state') = statePop state
    in
        execute op state'

--
-- Pop an interpreter and a program from the stack and
-- compose an operation that has the effect of running
-- that program on that interpreter.
--

opCreateOp state =
    let
        ((Interpreter interp), state') = statePop state
        (string, state'') = statePopString state'
        op = Program string interp
        state''' = statePush state'' (Operation op)
    in
        do return state'''

--
-- Pop an operation from the stack and push a program,
-- then an interpreter, onto the stack.  The semantics
-- of running that program with that interpreter will
-- be identical to the semantics of executing the operation.
-- However, the operation need not have been defined with
-- that program or that interpreter.  (This means one can
-- sensibly expand intrinsic operations.)
--

opExpandOp state =
    let
        ((Operation op), state') = statePop state
        (prog, interp) = expandOp op
        state'' = statePushString state' prog
        state''' = statePush state'' (Interpreter interp)
    in
        do return state'''


--
-- Stack manipulation.
--

--
-- Discard the top element of the stack.
--

opDiscard state =
    let
        (_, state') = statePop state
    in
        do return state'

--
-- Duplicate the top element of the stack.
--

opDuplicate state =
    let
        (elem, _) = statePop state
        state' = statePush state elem
    in
        do return state'

--
-- Swaps the top two elements of the stack.
--

opSwap state =
    let
        (elem_top, state') = statePop state
        (elem_bot, state'') = statePop state'
        state''' = statePush state'' elem_top
        state'''' = statePush state''' elem_bot
    in
        do return state''''

--
-- I/O.
--

opInput state = do
    symbol <- getCh state
    return (statePush state (Symbol symbol))

opOutput state =
    let
        ((Symbol symbol), state') = statePop state
    in do
        putCh state symbol
        return state'

--
-- Parameterizable operations.
--

opPushValue value state =
    do return (statePush state value)

opPushSymbol symbol state =
    opPushValue (Symbol symbol) state

opPushAndRetreat symbol state =
    let
        state' = statePush state (Symbol symbol)
        interp = getInterpreter state'
        interp' = getParent interp
        state'' = setInterpreter state' interp'
    in
        do return state''

--
-- Quote stuff.
--

opDescendQuote state =
    let
        state' = setInterpreter state deepQuoteInterpreter
        state'' = statePush state' (Symbol '[')
    in
        do return state''
    where
        deepQuoteInterpreter = Interp
            DeepQuote
            (Map.fromList
                ([(sym, (Intrinsic sym (opPushSymbol sym))) |
                   sym <- [(Char.chr 0) .. (Char.chr 255)]]
                 ++
                 [('[', (Intrinsic '[' opDescendQuote)),
                  (']', (Intrinsic ']' opAscendQuote))])
            )
            (Intrinsic ' ' opNop)
            (getInterpreter state)

opAscendQuote state =
    let
        interp = getInterpreter state
        interp' = getParent interp
        state' = setInterpreter state interp'
        state'' = statePush state' (Symbol ']')
    in
        do return state''

opSingleQuote state =
    let
        state' = setInterpreter state singleQuoteInterpreter
    in
        do return state'
    where
        singleQuoteInterpreter = Interp
            SingleQuote
            (Map.fromList
                [(sym, (Intrinsic sym (opPushAndRetreat sym))) |
                  sym <- [(Char.chr 0) .. (Char.chr 255)]]
            )
            (Intrinsic ' ' opNop)
            (getInterpreter state)


-----------------------------------------------------------------------
-- ===================== Debugging Functions ======================= --
-----------------------------------------------------------------------

type Debugger = [Symbol] -> State -> IO ()

nullDebugger p s = do
    return ()

stdDebugger program@(instr:rest) state = do
    putStr "\n"
    putStr ("Instr:  " ++ [instr] ++ "\n")
    putStr ("Rest:   " ++ rest ++ "\n")
    putStr ("Stack:  " ++ (show (stack state)) ++ "\n")
    putStr ("Interp: " ++ (show (interpreter state)) ++ "\n")
    putStr "(press ENTER) "
    control <- getCh state
    return ()


-----------------------------------------------------------------------
-- ====================== Top-Level Function ======================= --
-----------------------------------------------------------------------

initialInterpreter = Interp
  Initial
  (Map.fromList
    [
        ('v', (Intrinsic 'v' opReify)),
        ('^', (Intrinsic '^' opDiefy)),
        ('>', (Intrinsic '>' opExtractOp)),
        ('<', (Intrinsic '<' opInstallOp)),
        ('{', (Intrinsic '{' opGetParent)),
        ('}', (Intrinsic '}' opSetParent)),
        ('*', (Intrinsic '*' opCreateOp)),
        ('@', (Intrinsic '@' opExpandOp)),
        ('!', (Intrinsic '!' opPerform)),

        ('0', (Intrinsic '0' opPushNullInterpreter)),
        ('1', (Intrinsic '1' opMakeUniformInterpreter)),

        ('[', (Intrinsic '[' opDescendQuote)),
        ('\'', (Intrinsic '\'' opSingleQuote)),

        ('.', (Intrinsic '.' opOutput)),
        (',', (Intrinsic ',' opInput)),

        (':', (Intrinsic ':' opDuplicate)),
        ('$', (Intrinsic '$' opDiscard)),
        ('/', (Intrinsic '/' opSwap))
    ]
  )
  (Intrinsic ' ' opNop)
  NoInterp

initialState = State{ stack=(Stack []), interpreter=NoInterp, debugger=nullDebugger, getCh=getChar, putCh=putChar }

runWith string state =
    execute (Program string initialInterpreter) state

mascarpone string =
    runWith string initialState

mascarponeWithIO getCh putCh string =
    runWith string initialState{ getCh=getCh, putCh=putCh }

debug string =
    runWith string initialState{ debugger=stdDebugger }


-----------------------------------------------------------------------
-- ========================== Test Cases =========================== --
-----------------------------------------------------------------------

--
-- Drivers for test cases.  'demo' runs them straight, whereas 'test'
-- uses the debugger.
--

demo n = mascarpone (testProg n)

test n = debug (testProg n)


--
-- Test nesting quotes.
--

testProg 1 = "[o[ll]eh]........."

--
-- Make a new operation, defined as ",.", and execute it.
--

testProg 2 = "[,.]v*!"

--
-- Redefine "&" as ",." in the current interpreter, and try it.
--

testProg 3 = "v[,.]v*'&<^&&&"

--
-- Like testProg 3, but restore the old interpreter afterwards.
--

testProg 4 = "vv[,.]v*'&<^&&&^&&"

--
-- Define an operation that modifies the caller's interpreter.
-- The operation & causes m to be redefined as ",.".
--

testProg 5 = "v[v{[,.]v*'m<v}^]v*'&<^mmmmm&mm"

--
-- Execute an infinite loop.
--

testProg 6 = "v[vv{'d>'d<^,.d]v*'d<^d"

--
-- Execute an infinite loop, "tail-recursively".
--

testProg 7 = "v[vv{'d>'d<^,.0v}^d]v*'d<^d"

--
-- "Capture" a value in an operation: given a value, push
-- an operation that pushes that value when executed.
--
-- We want to push the string
--    ['v]
-- onto the stack, where v is the value we were given.  So we:
--    push [
--    swap
--    push '
--    swap
--    push ]
-- Then we are ready to make the operation.
--

testProg 8 = "v['[/''/']v*]v*'?<^'p?!."

--
-- Treat an interpreter as a store.  Define S to mean,
-- pop a symbol, a value, and an interpreter, and push a new
-- interpreter where the symbol means "push that value."
-- Then define F to mean, pop a symbol and an interpreter,
-- then extract the operation so named and run it (pushing
-- the value stored.)
--

testProg 9 = "v['[/''/']v*]v*'?<^v[/?/<]v*'S<[>!]v*'F<^[]v*1'p'kS'kF."

--
-- Get whatever definition the interpreter sees fit to give
-- us for a symbol input from the user, and output it.
-- We define '?' as above first, and for the most interesting
-- output (with this particular implementation ;) the user
-- should enter '?' when the time comes for ',' to execute...
--

testProg 10 = "v['[/''/']v*]v*'?<^v,>@$............"

--
-- Demonstrates how one can use * after @.
--

testProg 11 = "v['[/''/']v*]v*'?<^vv'?>@$v*'?<^'k?!."

--
-- Demonstrate that we cannot make an interpreter which is
-- its own parent.  Setting the parent of an interpreter
-- does not modify that interpreter; it produces a copy.
--

testProg 12 = "vv}^'k."
