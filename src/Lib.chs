{-# LANGUAGE ForeignFunctionInterface #-}
module Lib where

#include "raylib.h"
#include "wrapper.h"

{# pointer *Image foreign finalizer WrappedUnloadImage as unloadImage newtype #}

{# fun unsafe InitWindow as ^
    {`Int', `Int', `String'} -> `()' #}

{# fun unsafe CloseWindow as ^
    {} -> `()' #}

{# fun unsafe WindowShouldClose as ^
    {} -> `Bool' #}

{# fun unsafe IsWindowMinimized as ^
    {} -> `Int' #}

{# fun unsafe ToggleFullscreen as ^
    {} -> `()' #}

{# fun unsafe SetWindowIcon as ^
    {%`Image'} -> `()' #}

{# fun unsafe WrappedLoadImage as loadImage
    {`String'} -> `Image' #}

{# fun unsafe GetScreenWidth as ^
    {} -> `Int' #}
