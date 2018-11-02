{-# LANGUAGE ForeignFunctionInterface #-}
module Internal.Types where
import Data.Coerce (coerce)
import Data.Word (Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Storable (Storable)

#include "raylib.h"
#include "types_wrapper.h"

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
-- Keyboard Keys
-----------------------------------------

{# enum define KeyboardKey
    { KEY_SPACE as Space
    , KEY_ESCAPE as Escape
    , KEY_ENTER as Enter
    , KEY_TAB as Tab
    , KEY_BACKSPACE as Backspace
    , KEY_INSERT as Insert
    , KEY_DELETE as Delete
    , KEY_RIGHT as ArrowRight
    , KEY_LEFT as ArrowLeft
    , KEY_DOWN as ArrowDown
    , KEY_UP as ArrowUp
    , KEY_PAGE_UP as PageUp
    , KEY_PAGE_DOWN as PageDown
    , KEY_HOME as Home
    , KEY_END as End
    , KEY_CAPS_LOCK as CapsLock
    , KEY_SCROLL_LOCK as ScrollLock
    , KEY_NUM_LOCK as NumLock
    , KEY_PRINT_SCREEN as PrintScreen
    , KEY_PAUSE as Pause
    , KEY_F1 as F1
    , KEY_F2 as F2
    , KEY_F3 as F3
    , KEY_F4 as F4
    , KEY_F5 as F5
    , KEY_F6 as F6
    , KEY_F7 as F7
    , KEY_F8 as F8
    , KEY_F9 as F9
    , KEY_F10 as F10
    , KEY_F11 as F11
    , KEY_F12 as F12
    , KEY_LEFT_SHIFT as LeftShift
    , KEY_LEFT_CONTROL as LeftControl
    , KEY_LEFT_ALT as LeftAlt
    , KEY_RIGHT_SHIFT as RightShift
    , KEY_RIGHT_CONTROL as RightControl
    , KEY_RIGHT_ALT as RightAlt
    , KEY_GRAVE as Grave
    , KEY_SLASH as Slash
    , KEY_BACKSLASH as Backslash
    , KEY_ZERO as Zero
    , KEY_ONE as One
    , KEY_TWO as Two
    , KEY_THREE as Three
    , KEY_FOUR as Four
    , KEY_FIVE as Five
    , KEY_SIX as Six
    , KEY_SEVEN as Seven
    , KEY_EIGHT as Eight
    , KEY_NINE as Nine
    , KEY_A as A
    , KEY_B as B
    , KEY_C as C
    , KEY_D as D
    , KEY_E as E
    , KEY_F as F
    , KEY_G as G
    , KEY_H as H
    , KEY_I as I
    , KEY_J as J
    , KEY_K as K
    , KEY_L as L
    , KEY_M as M
    , KEY_N as N
    , KEY_O as O
    , KEY_P as P
    , KEY_Q as Q
    , KEY_R as R
    , KEY_S as S
    , KEY_T as T
    , KEY_U as U
    , KEY_V as V
    , KEY_W as W
    , KEY_X as X
    , KEY_Y as Y
    , KEY_Z as Z
    } deriving (Show, Eq) #}

-----------------------------------------
-- Mouse Buttons
-----------------------------------------

{# enum define MouseButton
    { MOUSE_LEFT_BUTTON as LeftButton
    , MOUSE_RIGHT_BUTTON as RightButton
    , MOUSE_MIDDLE_BUTTON as MiddleButton
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
-- Camera Mode
-----------------------------------------

{# enum CameraType
    { CAMERA_PERSPECTIVE as Perspective
    , CAMERA_ORTHOGRAPHIC as Orthographic
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

data Vector2 = Vector2 Float Float

instance Storable Vector2 where
    sizeOf = const {# sizeof Vector2 #}
    alignment = const {# alignof Vector2 #}
    peek p = do
        x <- realToFrac <$> {# get Vector2.x #} p
        y <- realToFrac <$> {# get Vector2.y #} p
        pure $ Vector2 x y
    poke p (Vector2 x y) = do
        {# set Vector2.x #} p (realToFrac x)
        {# set Vector2.y #} p (realToFrac y)

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
-- Rectangle
-----------------------------------------

{# pointer *Rectangle newtype #}

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
