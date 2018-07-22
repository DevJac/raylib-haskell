{-# LANGUAGE ForeignFunctionInterface #-}
module Lib where
import Data.Coerce (coerce)
import Foreign.ForeignPtr (withForeignPtr)

#include "raylib.h"
#include "wrapper.h"

{# pointer *Image foreign finalizer WrappedUnloadImage as unloadImage newtype #}

imageWidth :: Image -> IO Int
imageWidth image = fromIntegral <$> withForeignPtr (coerce image) width
  where width = {# get Image.width #}

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
