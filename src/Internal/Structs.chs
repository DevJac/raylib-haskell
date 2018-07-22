{-# LANGUAGE ForeignFunctionInterface #-}
module Internal.Structs where
import Data.Coerce (coerce)
import Foreign.ForeignPtr (withForeignPtr)

#include "raylib.h"
#include "structs_wrapper.h"

{# pointer *Image foreign finalizer WrappedUnloadImage as unloadImage newtype #}

imageWidth :: Image -> IO Int
imageWidth image = fromIntegral <$> withForeignPtr (coerce image) width
             where width = {# get Image.width #}

imageHeight :: Image -> IO Int
imageHeight image = fromIntegral <$> withForeignPtr (coerce image) height
                    where height = {# get Image.height #}
