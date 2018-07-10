{-# LANGUAGE ForeignFunctionInterface #-}
module Lib where

#include "raylib.h"

{# fun unsafe InitWindow as ^
    {`Int', `Int', `String'} -> `()' #}

{# fun unsafe CloseWindow as ^
    {} -> `()' #}

{# fun unsafe WindowShouldClose as ^
    {} -> `Bool' #}
