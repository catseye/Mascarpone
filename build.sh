#!/bin/sh

PROG=mascarpone

if command -v ghc >/dev/null 2>&1; then
    echo "building $PROG.exe with ghc"
    (cd src && ghc --make Main.hs -o ../bin/$PROG.exe)
else
    echo "ghc not found, not building $PROG.exe"
fi

# For this to work, you need hastec installed.

if command -v hastec_not_now_murray_not_now >/dev/null 2>&1; then
    echo "building $PROG.js with hastec"
    (cd src && hastec --make HasteMain.hs -o ../demo/$PROG.js)
else
    echo "hastec not found, not building $PROG.js"
fi
