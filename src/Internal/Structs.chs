{-# LANGUAGE ForeignFunctionInterface #-}
module Internal.Structs where
import Data.Coerce (coerce)
import Data.Word (Word8)
import Foreign.ForeignPtr (withForeignPtr)

#include "raylib.h"
#include "structs_wrapper.h"

-----------------------------------------
-- Config Flags
-----------------------------------------

{# enum define ConfigFlag
    { FLAG_SHOW_LOGO as ShowLogo
    , FLAG_FULLSCREEN_MODE as Fullscreen
    , FLAG_WINDOW_RESIZABLE as WindowResizable
    , FLAG_WINDOW_UNDECORATED as WindowUndecorated
    , FLAG_WINDOW_TRANSPARENT as WindowTransparent
    , FLAG_MSAA_4X_HINT as Msaa4xHint
    , FLAG_VSYNC_HINT as VsyncHint
    } deriving (Show, Eq) #}

-----------------------------------------
-- LogType
-----------------------------------------

{# enum LogType
    { LOG_INFO as Info
    , LOG_WARNING as Warning
    , LOG_ERROR as Error
    , LOG_DEBUG as Debug
    , LOG_OTHER as Other
    } deriving (Show, Eq) #}

-----------------------------------------
-- Color
-----------------------------------------

{# pointer *Color newtype #}

colorR :: Color -> IO Word8
colorR color = fromIntegral <$> r color
  where r = {# get Color.r #}

colorG :: Color -> IO Word8
colorG color = fromIntegral <$> g color
  where g = {# get Color.g #}

colorB :: Color -> IO Word8
colorB color = fromIntegral <$> b color
  where b = {# get Color.b #}

colorA :: Color -> IO Word8
colorA color = fromIntegral <$> a color
  where a = {# get Color.a #}

-----------------------------------------
-- Vector2
-----------------------------------------

{# pointer *Vector2 newtype #}

-----------------------------------------
-- Vector3
-----------------------------------------

{# pointer *Vector3 newtype #}

-----------------------------------------
-- Vector4
-----------------------------------------

{# pointer *Vector4 newtype #}

-----------------------------------------
-- Matrix
-----------------------------------------

{# pointer *Matrix newtype #}

-----------------------------------------
-- Image
-----------------------------------------

{# pointer *Image foreign finalizer WrappedUnloadImage as unloadImage newtype #}

imageWidth :: Image -> IO Int
imageWidth image = fromIntegral <$> withForeignPtr (coerce image) width
  where width = {# get Image.width #}

imageHeight :: Image -> IO Int
imageHeight image = fromIntegral <$> withForeignPtr (coerce image) height
  where height = {# get Image.height #}

-----------------------------------------
-- RenderTexture2D
-----------------------------------------

{# pointer *RenderTexture2D foreign finalizer WrappedUnloadRenderTexture as unloadRenderTexture newtype #}

-----------------------------------------
-- Camera3D
-----------------------------------------

{# pointer *Camera3D newtype #}

-----------------------------------------
-- Camera2D
-----------------------------------------

{# pointer *Camera2D newtype #}

-----------------------------------------
-- Ray
-----------------------------------------

{# pointer *Ray newtype #}