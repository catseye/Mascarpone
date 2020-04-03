#!/bin/sh

find . -name "*.o"  -exec rm {} \;
find . -name "*.hi" -exec rm {} \;
find . -name "*.jsmod" -exec rm {} \;
find . -name "*.exe" -exec rm {} \;
