{-# LANGUAGE ForeignFunctionInterface #-}
module Internal.Mvp where

#include "raylib.h"

{# fun unsafe InitWindow as ^
  {`Int', `Int', `String'} -> `()' #}

{# fun unsafe CloseWindow as ^
  {} -> `()' #}
