#!/usr/bin/env bash

# This script is for generating the intermediate c2hs files. This is helpful for debugging.
# This script is not part of the normal build process. It is for debugging purposes only.

cd src
c2hs Internal/Types.chs -C "-I../raylib/src"
