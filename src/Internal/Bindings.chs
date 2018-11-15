{- We will use a single c2hs file for this entire project. This is due to a couple of c2hs bugs:

   - {# import ... #} breaks implicit c2hs imports, which makes using multiple c2hs modules hard.
     See: https://github.com/haskell/c2hs/issues/189

   - The finalizers defined by {# pointer ... finalizer ... #} do not work across modules.
     See: https://github.com/haskell/c2hs/issues/174
-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Internal.Bindings where
import Data.Word (Word8, Word32)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with, toBool, fromBool)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (Storable(peek, poke))

#include "raylib.h"
#include "cbits.h"

---------------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------------

-- | Constructors:
--
-- @
-- ShowLogo
-- Fullscreen
-- WindowResizable
-- WindowUndecorated
-- WindowTransparent
-- Msaa4xHint
-- VsyncHint
-- @
{# enum define ConfigFlag
  { FLAG_SHOW_LOGO as ShowLogo
  , FLAG_FULLSCREEN_MODE as Fullscreen
  , FLAG_WINDOW_RESIZABLE as WindowResizable
  , FLAG_WINDOW_UNDECORATED as WindowUndecorated
  , FLAG_WINDOW_TRANSPARENT as WindowTransparent
  , FLAG_MSAA_4X_HINT as Msaa4xHint
  , FLAG_VSYNC_HINT as VsyncHint
  } deriving (Show, Eq) #}

-- | This is a workaround for (what seems to be) a Haddock parsing bug
--
-- @
-- Haddock! Why you do this!?!
-- @
data HaddockHelper

-- | Constructors:
--
-- @
-- Info
-- Warning
-- Error
-- Debug
-- Other
-- @
{# enum LogType
  { LOG_INFO as Info
  , LOG_WARNING as Warning
  , LOG_ERROR as Error
  , LOG_DEBUG as Debug
  , LOG_OTHER as Other
  } deriving (Show, Eq) #}

-- | Constructors:
--
-- @
-- Perspective
-- Orthographic
-- @
{# enum CameraType
  { CAMERA_PERSPECTIVE as Perspective
  , CAMERA_ORTHOGRAPHIC as Orthographic
  } deriving (Show, Eq) #}

-- | Constructors:
--
-- @
-- Space
-- Escape
-- Enter
-- Tab
-- Backspace
-- Insert
-- Delete
-- ArrowRight
-- ArrowLeft
-- ArrowDown
-- ArrowUp
-- PageUp
-- PageDown
-- Home
-- End
-- CapsLock
-- ScrollLock
-- NumLock
-- PrintScreen
-- Pause
-- F1
-- F2
-- F3
-- F4
-- F5
-- F6
-- F7
-- F8
-- F9
-- F10
-- F11
-- F12
-- LeftShift
-- LeftControl
-- LeftAlt
-- RightShift
-- RightControl
-- RightAlt
-- Grave
-- Slash
-- Backslash
-- Zero
-- One
-- Two
-- Three
-- Four
-- Five
-- Six
-- Seven
-- Eight
-- Nine
-- A
-- B
-- C
-- D
-- E
-- F
-- G
-- H
-- I
-- J
-- K
-- L
-- M
-- N
-- O
-- P
-- Q
-- R
-- S
-- T
-- U
-- V
-- W
-- X
-- Y
-- Z
-- @
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

-- | Constructors:
--
-- @
-- LeftButton
-- RightButton
-- MiddleButton
-- @
{# enum define MouseButton
  { MOUSE_LEFT_BUTTON as LeftButton
  , MOUSE_RIGHT_BUTTON as RightButton
  , MOUSE_MIDDLE_BUTTON as MiddleButton
  } deriving (Show, Eq) #}

-- | Constructors:
--
-- @
-- Gamepad1
-- Gamepad2
-- Gamepad3
-- Gamepad4
-- @
{# enum define Gamepad
  { GAMEPAD_PLAYER1 as Gamepad1
  , GAMEPAD_PLAYER2 as Gamepad2
  , GAMEPAD_PLAYER3 as Gamepad3
  , GAMEPAD_PLAYER4 as Gamepad4
  } deriving (Show, Eq) #}

-- | Constructors:
--
-- @
-- Ps3Triangle
-- Ps3Cricle
-- Ps3Cross
-- Ps3Square
-- Ps3L1
-- Ps3R1
-- Ps3L2
-- Ps3R2
-- Ps3Start
-- Ps3Select
-- Ps3Up
-- Ps3Right
-- Ps3Down
-- Ps3Left
-- Ps3PS
-- XBoxA
-- XBoxB
-- XBoxX
-- XBoxY
-- XBoxLB
-- XBoxRB
-- XBoxSelect
-- XBoxStart
-- XBoxUp
-- XBoxRight
-- XBoxDown
-- XBoxLeft
-- XBoxHome
-- AndroidUp
-- AndroidDown
-- AndroidLeft
-- AndroidRight
-- AndroidCenter
-- AndroidA
-- AndroidB
-- AndroidC
-- AndroidX
-- AndroidY
-- AndroidZ
-- AndroidL1
-- AndroidR1
-- AndroidL2
-- AndroidR2
-- @
{# enum define GamepadButton
  { GAMEPAD_PS3_BUTTON_TRIANGLE as Ps3Triangle
  , GAMEPAD_PS3_BUTTON_CIRCLE as Ps3Cricle
  , GAMEPAD_PS3_BUTTON_CROSS as Ps3Cross
  , GAMEPAD_PS3_BUTTON_SQUARE as Ps3Square
  , GAMEPAD_PS3_BUTTON_L1 as Ps3L1
  , GAMEPAD_PS3_BUTTON_R1 as Ps3R1
  , GAMEPAD_PS3_BUTTON_L2 as Ps3L2
  , GAMEPAD_PS3_BUTTON_R2 as Ps3R2
  , GAMEPAD_PS3_BUTTON_START as Ps3Start
  , GAMEPAD_PS3_BUTTON_SELECT as Ps3Select
  , GAMEPAD_PS3_BUTTON_UP as Ps3Up
  , GAMEPAD_PS3_BUTTON_RIGHT as Ps3Right
  , GAMEPAD_PS3_BUTTON_DOWN as Ps3Down
  , GAMEPAD_PS3_BUTTON_LEFT as Ps3Left
  , GAMEPAD_PS3_BUTTON_PS as Ps3PS
  , GAMEPAD_XBOX_BUTTON_A as XBoxA
  , GAMEPAD_XBOX_BUTTON_B as XBoxB
  , GAMEPAD_XBOX_BUTTON_X as XBoxX
  , GAMEPAD_XBOX_BUTTON_Y as XBoxY
  , GAMEPAD_XBOX_BUTTON_LB as XBoxLB
  , GAMEPAD_XBOX_BUTTON_RB as XBoxRB
  , GAMEPAD_XBOX_BUTTON_SELECT as XBoxSelect
  , GAMEPAD_XBOX_BUTTON_START as XBoxStart
  , GAMEPAD_XBOX_BUTTON_UP as XBoxUp
  , GAMEPAD_XBOX_BUTTON_RIGHT as XBoxRight
  , GAMEPAD_XBOX_BUTTON_DOWN as XBoxDown
  , GAMEPAD_XBOX_BUTTON_LEFT as XBoxLeft
  , GAMEPAD_XBOX_BUTTON_HOME as XBoxHome
  , GAMEPAD_ANDROID_DPAD_UP as AndroidUp
  , GAMEPAD_ANDROID_DPAD_DOWN as AndroidDown
  , GAMEPAD_ANDROID_DPAD_LEFT as AndroidLeft
  , GAMEPAD_ANDROID_DPAD_RIGHT as AndroidRight
  , GAMEPAD_ANDROID_DPAD_CENTER as AndroidCenter
  , GAMEPAD_ANDROID_BUTTON_A as AndroidA
  , GAMEPAD_ANDROID_BUTTON_B as AndroidB
  , GAMEPAD_ANDROID_BUTTON_C as AndroidC
  , GAMEPAD_ANDROID_BUTTON_X as AndroidX
  , GAMEPAD_ANDROID_BUTTON_Y as AndroidY
  , GAMEPAD_ANDROID_BUTTON_Z as AndroidZ
  , GAMEPAD_ANDROID_BUTTON_L1 as AndroidL1
  , GAMEPAD_ANDROID_BUTTON_R1 as AndroidR1
  , GAMEPAD_ANDROID_BUTTON_L2 as AndroidL2
  , GAMEPAD_ANDROID_BUTTON_R2 as AndroidR2
  } deriving (Show) #}

instance Eq GamepadButton where
  x == y = (fromEnum x) == (fromEnum y)

-- | Constructors:
--
-- @
-- Ps3LeftX
-- Ps3LeftY
-- Ps3RightX
-- Ps3RightY
-- Ps3L2Axis
-- Ps3R2Axis
-- XBoxLeftX
-- XBoxLeftY
-- XBoxRightX
-- XBoxRightY
-- XBoxLT
-- XBoxRT
-- @
{# enum define GamepadAxis
  { GAMEPAD_PS3_AXIS_LEFT_X as Ps3LeftX
  , GAMEPAD_PS3_AXIS_LEFT_Y as Ps3LeftY
  , GAMEPAD_PS3_AXIS_RIGHT_X as Ps3RightX
  , GAMEPAD_PS3_AXIS_RIGHT_Y as Ps3RightY
  , GAMEPAD_PS3_AXIS_L2 as Ps3L2Axis
  , GAMEPAD_PS3_AXIS_R2 as Ps3R2Axis
  , GAMEPAD_XBOX_AXIS_LEFT_X as XBoxLeftX
  , GAMEPAD_XBOX_AXIS_LEFT_Y as XBoxLeftY
  , GAMEPAD_XBOX_AXIS_RIGHT_X as XBoxRightX
  , GAMEPAD_XBOX_AXIS_RIGHT_Y as XBoxRightY
  , GAMEPAD_XBOX_AXIS_LT as XBoxLT
  , GAMEPAD_XBOX_AXIS_RT as XBoxRT
  } deriving (Show) #}

instance Eq GamepadAxis where
  x == y = (fromEnum x) == (fromEnum y)

-- | Constructors:
--
-- @
-- None
-- Tap
-- DoubleTap
-- Hold
-- Drag
-- SwipeRight
-- SwipeLeft
-- SwipeUp
-- SwipeDown
-- PinchIn
-- PinchOut
-- @
{# enum Gestures as Gesture
  { GESTURE_NONE as None
  , GESTURE_TAP as Tap
  , GESTURE_DOUBLETAP as DoubleTap
  , GESTURE_HOLD as Hold
  , GESTURE_DRAG as Drag
  , GESTURE_SWIPE_RIGHT as SwipeRight
  , GESTURE_SWIPE_LEFT as SwipeLeft
  , GESTURE_SWIPE_UP as SwipeUp
  , GESTURE_SWIPE_DOWN as SwipeDown
  , GESTURE_PINCH_IN as PinchIn
  , GESTURE_PINCH_OUT as PinchOut
  } deriving (Show, Eq) #}

---------------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------------

-- | @Color r g b a@
data Color = Color !Word8 !Word8 !Word8 !Word8 deriving (Show, Eq)

{# pointer *Color as ColorPtr -> Color #}

instance Storable Color where
  sizeOf _ = {# sizeof Color #}
  alignment _ = {# alignof Color #}
  peek p = do
    r <- fromIntegral <$> {# get Color.r #} p
    g <- fromIntegral <$> {# get Color.g #} p
    b <- fromIntegral <$> {# get Color.b #} p
    a <- fromIntegral <$> {# get Color.a #} p
    pure $ Color r g b a
  poke p (Color r g b a) = do
    {# set Color.r #} p (fromIntegral r)
    {# set Color.g #} p (fromIntegral g)
    {# set Color.b #} p (fromIntegral b)
    {# set Color.a #} p (fromIntegral a)

-- | @Rectangle x y width height@
data Rectangle = Rectangle !Float !Float !Float !Float deriving (Show, Eq)

{# pointer *Rectangle as RectanglePtr -> Rectangle #}

instance Storable Rectangle where
  sizeOf _ = {# sizeof Rectangle #}
  alignment _ = {# alignof Rectangle #}
  peek p = do
    x      <- realToFrac <$> {# get Rectangle.x #}      p
    y      <- realToFrac <$> {# get Rectangle.y #}      p
    width  <- realToFrac <$> {# get Rectangle.width #}  p
    height <- realToFrac <$> {# get Rectangle.height #} p
    pure $ Rectangle x y width height
  poke p (Rectangle x y width height) = do
    {# set Rectangle.x #}      p (realToFrac x)
    {# set Rectangle.y #}      p (realToFrac y)
    {# set Rectangle.width #}  p (realToFrac width)
    {# set Rectangle.height #} p (realToFrac height)

-- | @Vector2 x y@
data Vector2 = Vector2 !Float !Float deriving (Show, Eq)

{# pointer *Vector2 as Vector2Ptr -> Vector2 #}

instance Storable Vector2 where
  sizeOf _ = {# sizeof Vector2 #}
  alignment _ = {# alignof Vector2 #}
  peek p = do
    x <- realToFrac <$> {# get Vector2.x #} p
    y <- realToFrac <$> {# get Vector2.y #} p
    pure $ Vector2 x y
  poke p (Vector2 x y) = do
    {# set Vector2.x #} p (realToFrac x)
    {# set Vector2.y #} p (realToFrac y)

-- | @Vector3 x y z@
data Vector3 = Vector3 !Float !Float !Float deriving (Show, Eq)

{# pointer *Vector3 as Vector3Ptr -> Vector3 #}

instance Storable Vector3 where
  sizeOf _ = {# sizeof Vector3 #}
  alignment _ = {# alignof Vector3 #}
  peek p = do
    x <- realToFrac <$> {# get Vector3.x #} p
    y <- realToFrac <$> {# get Vector3.y #} p
    z <- realToFrac <$> {# get Vector3.z #} p
    pure $ Vector3 x y z
  poke p (Vector3 x y z) = do
    {# set Vector3.x #} p (realToFrac x)
    {# set Vector3.y #} p (realToFrac y)
    {# set Vector3.z #} p (realToFrac z)

-- | @Vector4 x y z w@
--
-- This is also known as a 'Quaternion' in raylib.
data Vector4 = Vector4 !Float !Float !Float !Float deriving (Show, Eq)

type Quaternion = Vector4

{# pointer *Vector4 as Vector4Ptr -> Vector4 #}

instance Storable Vector4 where
  sizeOf _ = {# sizeof Vector4 #}
  alignment _ = {# alignof Vector4 #}
  peek p = do
    x <- realToFrac <$> {# get Vector4.x #} p
    y <- realToFrac <$> {# get Vector4.y #} p
    z <- realToFrac <$> {# get Vector4.z #} p
    w <- realToFrac <$> {# get Vector4.w #} p
    pure $ Vector4 x y z w
  poke p (Vector4 x y z w) = do
    {# set Vector4.x #} p (realToFrac x)
    {# set Vector4.y #} p (realToFrac y)
    {# set Vector4.z #} p (realToFrac z)
    {# set Vector4.w #} p (realToFrac w)

-- | @Matrix m0 m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 m13 m14 m15@
data Matrix = Matrix !Float !Float !Float !Float
                     !Float !Float !Float !Float
                     !Float !Float !Float !Float
                     !Float !Float !Float !Float deriving (Show, Eq)

{# pointer *Matrix as MatrixPtr -> Matrix #}

instance Storable Matrix where
  sizeOf _ = {# sizeof Matrix #}
  alignment _ = {# alignof Matrix #}
  peek p = do
    m0  <- realToFrac <$> {# get Matrix.m0  #} p
    m1  <- realToFrac <$> {# get Matrix.m1  #} p
    m2  <- realToFrac <$> {# get Matrix.m2  #} p
    m3  <- realToFrac <$> {# get Matrix.m3  #} p
    m4  <- realToFrac <$> {# get Matrix.m4  #} p
    m5  <- realToFrac <$> {# get Matrix.m5  #} p
    m6  <- realToFrac <$> {# get Matrix.m6  #} p
    m7  <- realToFrac <$> {# get Matrix.m7  #} p
    m8  <- realToFrac <$> {# get Matrix.m8  #} p
    m9  <- realToFrac <$> {# get Matrix.m9  #} p
    m10 <- realToFrac <$> {# get Matrix.m10 #} p
    m11 <- realToFrac <$> {# get Matrix.m11 #} p
    m12 <- realToFrac <$> {# get Matrix.m12 #} p
    m13 <- realToFrac <$> {# get Matrix.m13 #} p
    m14 <- realToFrac <$> {# get Matrix.m14 #} p
    m15 <- realToFrac <$> {# get Matrix.m15 #} p
    pure $ Matrix m0  m1  m2  m3
                  m4  m5  m6  m7
                  m8  m9  m10 m11
                  m12 m13 m14 m15
  poke p (Matrix m0  m1  m2  m3
                 m4  m5  m6  m7
                 m8  m9  m10 m11
                 m12 m13 m14 m15) = do
    {# set Matrix.m0  #} p (realToFrac m0 )
    {# set Matrix.m1  #} p (realToFrac m1 )
    {# set Matrix.m2  #} p (realToFrac m2 )
    {# set Matrix.m3  #} p (realToFrac m3 )
    {# set Matrix.m4  #} p (realToFrac m4 )
    {# set Matrix.m5  #} p (realToFrac m5 )
    {# set Matrix.m6  #} p (realToFrac m6 )
    {# set Matrix.m7  #} p (realToFrac m7 )
    {# set Matrix.m8  #} p (realToFrac m8 )
    {# set Matrix.m9  #} p (realToFrac m9 )
    {# set Matrix.m10 #} p (realToFrac m10)
    {# set Matrix.m11 #} p (realToFrac m11)
    {# set Matrix.m12 #} p (realToFrac m12)
    {# set Matrix.m13 #} p (realToFrac m13)
    {# set Matrix.m14 #} p (realToFrac m14)
    {# set Matrix.m15 #} p (realToFrac m15)

-- | @Camera3D position target up fovy type@
data Camera3D = Camera3D !Vector3 !Vector3 !Vector3 !Float !Int deriving (Show, Eq)

{# pointer *Camera3D as Camera3DPtr -> Camera3D #}

instance Storable Camera3D where
  sizeOf _ = {# sizeof Camera3D #}
  alignment _ = {# alignof Camera3D #}
  peek p = do
    position <- peek $ p `plusPtr` {# offsetof Camera3D.position #}
    target   <- peek $ p `plusPtr` {# offsetof Camera3D.target #}
    up       <- peek $ p `plusPtr` {# offsetof Camera3D.up #}
    fovy     <- realToFrac   <$> {# get Camera3D.fovy #} p
    type_    <- fromIntegral <$> {# get Camera3D.type #} p
    pure $ Camera3D position target up fovy type_
  poke p (Camera3D position target up fovy type_) = do
    poke (p `plusPtr` {# offsetof Camera3D.position #}) position
    poke (p `plusPtr` {# offsetof Camera3D.target #})   target
    poke (p `plusPtr` {# offsetof Camera3D.up #})       up
    {# set Camera3D.fovy #} p (realToFrac fovy)
    {# set Camera3D.type #} p (fromIntegral type_)

-- | @Camera2D offset target rotation zoom@
data Camera2D = Camera2D !Vector2 !Vector2 !Float !Float deriving (Show, Eq)

{# pointer *Camera2D as Camera2DPtr -> Camera2D #}

instance Storable Camera2D where
  sizeOf _ = {# sizeof Camera2D #}
  alignment _ = {# alignof Camera2D #}
  peek p = do
    offset   <- peek $ p `plusPtr` {# offsetof Camera2D.offset #}
    target   <- peek $ p `plusPtr` {# offsetof Camera2D.target #}
    rotation <- realToFrac <$> {# get Camera2D.rotation #} p
    zoom     <- realToFrac <$> {# get Camera2D.zoom #}     p
    pure $ Camera2D offset target rotation zoom
  poke p (Camera2D offset target rotation zoom) = do
    poke (p `plusPtr` {# offsetof Camera2D.offset #}) offset
    poke (p `plusPtr` {# offsetof Camera2D.target #}) target
    {# set Camera2D.rotation #} p (realToFrac rotation)
    {# set Camera2D.zoom #}     p (realToFrac zoom)

-- | @Ray position direction@
data Ray = Ray !Vector3 !Vector3 deriving (Show, Eq)

{# pointer *Ray as RayPtr -> Ray #}

instance Storable Ray where
  sizeOf _ = {# sizeof Ray #}
  alignment _ = {# alignof Ray #}
  peek p = do
    position  <- peek $ p `plusPtr` {# offsetof Ray.position #}
    direction <- peek $ p `plusPtr` {# offsetof Ray.direction #}
    pure $ Ray position direction
  poke p (Ray position direction) = do
    poke (p `plusPtr` {# offsetof Ray.position #})  position
    poke (p `plusPtr` {# offsetof Ray.direction #}) direction

-- | @RayHitInfo hit distance position normal@
data RayHitInfo = RayHitInfo !Bool !Float !Vector3 !Vector3 deriving (Show, Eq)

{# pointer *RayHitInfo as RayHitInfoPtr -> RayHitInfo #}

instance Storable RayHitInfo where
  sizeOf _ = {# sizeof RayHitInfo #}
  alignment _ = {# sizeof RayHitInfo #}
  peek p = do
    hit      <- toBool     <$> {# get RayHitInfo.hit #}      p
    distance <- realToFrac <$> {# get RayHitInfo.distance #} p
    position <- peek $ p `plusPtr` {# offsetof RayHitInfo.position #}
    normal   <- peek $ p `plusPtr` {# offsetof RayHitInfo.normal #}
    pure $ RayHitInfo hit distance position normal
  poke p (RayHitInfo hit distance position normal) = do
    {# set RayHitInfo.hit #}      p (fromBool hit)
    {# set RayHitInfo.distance #} p (realToFrac distance)
    poke (p `plusPtr` {# offsetof RayHitInfo.position #}) position
    poke (p `plusPtr` {# offsetof RayHitInfo.normal #})   normal

{# pointer *Image foreign finalizer WrappedUnloadImage as unloadImage newtype #}

{# pointer *Texture2D foreign finalizer WrappedUnloadTexture as unloadTexture newtype #}

{# pointer *RenderTexture2D foreign finalizer WrappedUnloadRenderTexture as unloadRenderTexture newtype #}

{# pointer *Font foreign finalizer WrappedUnloadFont as unloadFont newtype #}

{# pointer *Model foreign finalizer WrappedUnloadModel as unloadModel newtype #}

-- raylib's UnloadMesh function takes a pointer, so we don't have to wrap it like the other Unload* functions.
{# pointer *Mesh foreign finalizer UnloadMesh as unloadMesh newtype #}

{# pointer *Material foreign finalizer WrappedUnloadMaterial as unloadMaterial newtype #}

{# pointer *Shader foreign finalizer WrappedUnloadShader as unloadShader newtype #}

{# pointer *Wave foreign finalizer WrappedUnloadWave as unloadWave newtype #}

{# pointer *Sound foreign finalizer WrappedUnloadSound as unloadSound newtype #}

{# pointer *Music foreign finalizer WrappedUnloadMusicStream as unloadMusicStream newtype #}

{# pointer *AudioStream foreign finalizer WrappedCloseAudioStream as closeAudioStream newtype #}

---------------------------------------------------------------------------------
-- Core
---------------------------------------------------------------------------------

{# fun unsafe InitWindow as ^
  {`Int', `Int', `String'} -> `()' #}

{# fun unsafe CloseWindow as ^
  {} -> `()' #}

{# fun unsafe IsWindowReady as ^
  {} -> `Bool' #}

{# fun unsafe WindowShouldClose as ^
  {} -> `Bool' #}

{# fun unsafe IsWindowMinimized as ^
  {} -> `Bool' #}

{# fun unsafe ToggleFullscreen as ^
  {} -> `()' #}

{# fun unsafe SetWindowIcon as ^
  {%`Image'} -> `()' #}

{# fun unsafe SetWindowTitle as ^
  {`String'} -> `()' #}

{# fun unsafe SetWindowPosition as ^
  {`Int', `Int'} -> `()' #}

{# fun unsafe SetWindowMonitor as ^
  {`Int'} -> `()' #}

{# fun unsafe SetWindowMinSize as ^
  {`Int', `Int'} -> `()' #}

{# fun unsafe SetWindowSize as ^
  {`Int', `Int'} -> `()' #}

{# fun unsafe GetScreenWidth as ^
  {} -> `Int' #}

{# fun unsafe GetScreenHeight as ^
  {} -> `Int' #}

{# fun unsafe ShowCursor as ^
  {} -> `()' #}

{# fun unsafe HideCursor as ^
  {} -> `()' #}

{# fun unsafe IsCursorHidden as ^
  {} -> `Bool' #}

{# fun unsafe EnableCursor as ^
  {} -> `()' #}

{# fun unsafe DisableCursor as ^
  {} -> `()' #}

{# fun unsafe ClearBackground as ^
  {with* %`Color'} -> `()' #}

{# fun unsafe BeginDrawing as ^
  {} -> `()' #}

{# fun unsafe EndDrawing as ^
  {} -> `()' #}

{# fun unsafe BeginMode2D as ^
  {with* %`Camera2D'} -> `()' #}

{# fun unsafe EndMode2D as ^
  {} -> `()' #}

{# fun unsafe BeginMode3D as ^
  {with* %`Camera3D'} -> `()' #}

{# fun unsafe EndMode3D as ^
  {} -> `()' #}

{# fun unsafe BeginTextureMode as ^
  {%`RenderTexture2D'} -> `()' #}

{# fun unsafe EndTextureMode as ^
  {} -> `()' #}

{# fun unsafe WrappedGetMouseRay as getMouseRay
  {with* %`Vector2', with* %`Camera3D', alloca- `Ray' peek*} -> `()' #}

{# fun unsafe WrappedGetWorldToScreen as getWorldToScreen
  {with* %`Vector3', with* %`Camera3D', alloca- `Vector2' peek*} -> `()' #}

{# fun unsafe WrappedGetCameraMatrix as getCameraMatrix
  {with* %`Camera3D', alloca- `Matrix' peek*} -> `()' #}

{# fun unsafe SetTargetFPS as ^
  {`Int'} -> `()' #}

{# fun unsafe GetFPS as ^
  {} -> `Int' #}

{# fun unsafe GetFrameTime as ^
  {} -> `Float' #}

{# fun unsafe GetTime as ^
  {} -> `Double' #}

{# fun unsafe ShowLogo as ^
  {} -> `()' #}

{# fun unsafe SetConfigFlags as ^
  {`Word8'} -> `()' #}

{# fun unsafe SetTraceLog as ^
  {`Word8'} -> `()' #}

{# fun unsafe TraceLog as ^
  {`Int', `String'} -> `()' #}

{# fun unsafe TakeScreenshot as ^
  {`String'} -> `()' #}

{# fun unsafe GetWorkingDirectory as ^
  {} -> `String' #}

{# fun unsafe ChangeDirectory as ^
  {`String'} -> `Bool' #}

{# fun unsafe IsFileDropped as ^
  {} -> `Bool' #}

{# fun unsafe GetDroppedFiles as ^
  {alloca- `CInt' peek*} -> `Ptr CString' id #}

{# fun unsafe ClearDroppedFiles as ^
  {} -> `()' #}

{# fun unsafe IsKeyPressed as ^
  {`Int'} -> `Bool' #}

{# fun unsafe IsKeyDown as ^
  {`Int'} -> `Bool' #}

{# fun unsafe IsKeyReleased as ^
  {`Int'} -> `Bool' #}

{# fun unsafe IsKeyUp as ^
  {`Int'} -> `Bool' #}

{# fun unsafe GetKeyPressed as ^
  {} -> `Int' #}

{# fun unsafe SetExitKey as ^
  {`Int'} -> `()' #}

{# fun unsafe IsGamepadAvailable as ^
  {`Int'} -> `Bool' #}

{# fun unsafe IsGamepadName as ^
  {`Int', `String'} -> `Bool' #}

{# fun unsafe GetGamepadName as ^
  {`Int'} -> `String' #}

{# fun unsafe IsGamepadButtonPressed as ^
  {`Int', `Int'} -> `Bool' #}

{# fun unsafe IsGamepadButtonDown as ^
  {`Int', `Int'} -> `Bool' #}

{# fun unsafe IsGamepadButtonReleased as ^
  {`Int', `Int'} -> `Bool' #}

{# fun unsafe IsGamepadButtonUp as ^
  {`Int', `Int'} -> `Bool' #}

{# fun unsafe GetGamepadButtonPressed as ^
  {} -> `Int' #}

{# fun unsafe GetGamepadAxisCount as ^
  {`Int'} -> `Int' #}

{# fun unsafe GetGamepadAxisMovement as ^
  {`Int', `Int'} -> `Float' #}

{# fun unsafe IsMouseButtonPressed as ^
  {`Int'} -> `Bool' #}

{# fun unsafe IsMouseButtonDown as ^
  {`Int'} -> `Bool' #}

{# fun unsafe IsMouseButtonReleased as ^
  {`Int'} -> `Bool' #}

{# fun unsafe IsMouseButtonUp as ^
  {`Int'} -> `Bool' #}

{# fun unsafe GetMouseX as ^
  {} -> `Int' #}

{# fun unsafe GetMouseY as ^
  {} -> `Int' #}

{# fun unsafe WrappedGetMousePosition as getMousePosition
  {alloca- `Vector2' peek*} -> `()' #}

{# fun unsafe SetMousePosition as ^
  {with* %`Vector2'} -> `()' #}

{# fun unsafe GetMouseWheelMove as ^
  {} -> `Int' #}

{# fun unsafe GetTouchX as ^
  {} -> `Int' #}

{# fun unsafe GetTouchY as ^
  {} -> `Int' #}

{# fun unsafe WrappedGetTouchPosition as getTouchPosition
  {`Int', alloca- `Vector2' peek*} -> `()' #}

{# fun unsafe SetGesturesEnabled as ^
  {`Word32'} -> `()' #}

{# fun unsafe IsGestureDetected as ^
  {`Int'} -> `Bool' #}

{# fun unsafe GetGestureDetected as ^
  {} -> `Int' #}

{# fun unsafe GetTouchPointsCount as ^
  {} -> `Int' #}

{# fun unsafe GetGestureHoldDuration as ^
  {} -> `Float' #}

{# fun unsafe WrappedGetGestureDragVector as getGestureDragVector
  {alloca- `Vector2' peek*} -> `()' #}

{# fun unsafe GetGestureDragAngle as ^
  {} -> `Float' #}

{# fun unsafe WrappedGetGesturePinchVector as getGesturePinchVector
  {alloca- `Vector2' peek*} -> `()' #}

{# fun unsafe GetGesturePinchAngle as ^
  {} -> `Float' #}

{# fun unsafe SetCameraMode as ^
  {with* %`Camera3D', `Int'} -> `()' #}

{# fun unsafe UpdateCamera as ^
  {`Camera3DPtr'} -> `()' #}

{# fun unsafe SetCameraPanControl as ^
  {`Int'} -> `()' #}

{# fun unsafe SetCameraAltControl as ^
  {`Int'} -> `()' #}

{# fun unsafe SetCameraSmoothZoomControl as ^
  {`Int'} -> `()' #}

{# fun unsafe SetCameraMoveControls as ^
  {`Int', `Int', `Int', `Int', `Int', `Int'} -> `()' #}

---------------------------------------------------------------------------------
-- Shapes
---------------------------------------------------------------------------------

{# fun unsafe DrawPixel as ^
  {`Int', `Int', with* %`Color'} -> `()' #}

{# fun unsafe DrawPixelV as ^
  {with* %`Vector2', with* %`Color'} -> `()' #}

{# fun unsafe DrawLine as ^
  {`Int', `Int', `Int', `Int', with* %`Color'} -> `()' #}

{# fun unsafe DrawLineV as ^
  {with* %`Vector2', with* %`Vector2', with* %`Color'} -> `()' #}

{# fun unsafe DrawLineEx as ^
  {with* %`Vector2', with* %`Vector2', `Float', with* %`Color'} -> `()' #}

{# fun unsafe DrawLineBezier as ^
  {with* %`Vector2', with* %`Vector2', `Float', with* %`Color'} -> `()' #}

{# fun unsafe DrawCircle as ^
  {`Int', `Int', `Float', with* %`Color'} -> `()' #}

{# fun unsafe DrawCircleGradient as ^
  {`Int', `Int', `Float', with* %`Color', with* %`Color'} -> `()' #}

{# fun unsafe DrawCircleV as ^
  {with* %`Vector2', `Float', with* %`Color'} -> `()' #}

{# fun unsafe DrawCircleLines as ^
  {`Int', `Int', `Float', with* %`Color'} -> `()' #}

{# fun unsafe DrawRectangle as ^
  {`Int', `Int', `Int', `Int', with* %`Color'} -> `()' #}

{# fun unsafe DrawRectangleV as ^
  {with* %`Vector2', with* %`Vector2', with* %`Color'} -> `()' #}

{# fun unsafe DrawRectangleRec as ^
  {with* %`Rectangle', with* %`Color'} -> `()' #}

{# fun unsafe DrawRectanglePro as ^
  {with* %`Rectangle', with* %`Vector2', `Float', with* %`Color'} -> `()' #}

{# fun unsafe DrawRectangleGradientV as ^
  {`Int', `Int', `Int', `Int', with* %`Color', with* %`Color'} -> `()' #}

{# fun unsafe DrawRectangleGradientH as ^
  {`Int', `Int', `Int', `Int', with* %`Color', with* %`Color'} -> `()' #}

{# fun unsafe DrawRectangleGradientEx as ^
  {with* %`Rectangle', with* %`Color', with* %`Color', with* %`Color', with* %`Color'} -> `()' #}

{# fun unsafe DrawRectangleLines as ^
  {`Int', `Int', `Int', `Int', with* %`Color'} -> `()' #}

{# fun unsafe DrawRectangleLinesEx as ^
  {with* %`Rectangle', `Int', with* %`Color'} -> `()' #}

{# fun unsafe DrawTriangle as ^
  {with* %`Vector2', with* %`Vector2', with* %`Vector2', with* %`Color'} -> `()' #}

{# fun unsafe DrawTriangleLines as ^
  {with* %`Vector2', with* %`Vector2', with* %`Vector2', with* %`Color'} -> `()' #}

{# fun unsafe DrawPoly as ^
  {with* %`Vector2', `Int', `Float', `Float', with* %`Color'} -> `()' #}

{# fun unsafe DrawPolyEx as ^
  {with* `Vector2', `Int', with* %`Color'} -> `()' #}

{# fun unsafe DrawPolyExLines as ^
  {with* `Vector2', `Int', with* %`Color'} -> `()' #}

{# fun unsafe CheckCollisionRecs as ^
  {with* %`Rectangle', with* %`Rectangle'} -> `Bool' #}

{# fun unsafe CheckCollisionCircles as ^
  {with* %`Vector2', `Float', with* %`Vector2', `Float'} -> `Bool' #}

{# fun unsafe CheckCollisionCircleRec as ^
  {with* %`Vector2', `Float', with* %`Rectangle'} -> `Bool' #}

{# fun unsafe WrappedGetCollisionRec as getCollisionRec
  {with* %`Rectangle', with* %`Rectangle', alloca- `Rectangle' peek*} -> `()' #}

{# fun unsafe CheckCollisionPointRec as ^
  {with* %`Vector2', with* %`Rectangle'} -> `Bool' #}

{# fun unsafe CheckCollisionPointCircle as ^
  {with* %`Vector2', with* %`Vector2', `Float'} -> `Bool' #}

{# fun unsafe CheckCollisionPointTriangle as ^
  {with* %`Vector2', with* %`Vector2', with* %`Vector2', with* %`Vector2'} -> `Bool' #}

---------------------------------------------------------------------------------
-- Textures
---------------------------------------------------------------------------------

{# fun unsafe WrappedLoadImage as loadImage
  {`String'} -> `Image' #}

-- TODO // Image/Texture2D data loading/unloading/saving functions
-- TODO Image LoadImageEx(Color *pixels, int width, int height);                                            // Load image from Color array data (RGBA - 32bit)
-- TODO Image LoadImagePro(void *data, int width, int height, int format);                                  // Load image from raw data with parameters
-- TODO Image LoadImageRaw(const char *fileName, int width, int height, int format, int headerSize);        // Load image from RAW file data
-- TODO void ExportImage(const char *fileName, Image image);                                                // Export image as a PNG file

{# fun unsafe WrappedLoadTexture as loadTexture
  {`String'} -> `Texture2D' #}

-- TODO Texture2D LoadTextureFromImage(Image image);                                                        // Load texture from image data
-- TODO RenderTexture2D LoadRenderTexture(int width, int height);                                           // Load texture for rendering (framebuffer)
-- TODO Color *GetImageData(Image image);                                                                   // Get pixel data from image as a Color struct array
-- TODO Vector4 *GetImageDataNormalized(Image image);                                                       // Get pixel data from image as Vector4 array (float normalized)
-- TODO int GetPixelDataSize(int width, int height, int format);                                            // Get pixel data size in bytes (image or texture)
-- TODO Image GetTextureData(Texture2D texture);                                                            // Get pixel data from GPU texture and return an Image
-- TODO void UpdateTexture(Texture2D texture, const void *pixels);                                          // Update GPU texture with new data

-- TODO // Image manipulation functions
-- TODO Image ImageCopy(Image image);                                                                       // Create an image duplicate (useful for transformations)
-- TODO void ImageToPOT(Image *image, Color fillColor);                                                     // Convert image to POT (power-of-two)
-- TODO void ImageFormat(Image *image, int newFormat);                                                      // Convert image data to desired format
-- TODO void ImageAlphaMask(Image *image, Image alphaMask);                                                 // Apply alpha mask to image
-- TODO void ImageAlphaClear(Image *image, Color color, float threshold);                                   // Clear alpha channel to desired color
-- TODO void ImageAlphaCrop(Image *image, float threshold);                                                 // Crop image depending on alpha value
-- TODO void ImageAlphaPremultiply(Image *image);                                                           // Premultiply alpha channel
-- TODO void ImageCrop(Image *image, Rectangle crop);                                                       // Crop an image to a defined rectangle
-- TODO void ImageResize(Image *image, int newWidth, int newHeight);                                        // Resize image (bilinear filtering)
-- TODO void ImageResizeNN(Image *image, int newWidth,int newHeight);                                       // Resize image (Nearest-Neighbor scaling algorithm)
-- TODO void ImageResizeCanvas(Image *image, int newWidth, int newHeight,
-- TODO                        int offsetX, int offsetY, Color color);                                      // Resize canvas and fill with color
-- TODO void ImageMipmaps(Image *image);                                                                    // Generate all mipmap levels for a provided image
-- TODO void ImageDither(Image *image, int rBpp, int gBpp, int bBpp, int aBpp);                             // Dither image data to 16bpp or lower (Floyd-Steinberg dithering)
-- TODO Image ImageText(const char *text, int fontSize, Color color);                                       // Create an image from text (default font)
-- TODO Image ImageTextEx(Font font, const char *text, float fontSize, float spacing, Color tint);          // Create an image from text (custom sprite font)
-- TODO void ImageDraw(Image *dst, Image src, Rectangle srcRec, Rectangle dstRec);                          // Draw a source image within a destination image
-- TODO void ImageDrawRectangle(Image *dst, Vector2 position, Rectangle rec, Color color);                  // Draw rectangle within an image
-- TODO void ImageDrawText(Image *dst, Vector2 position, const char *text, int fontSize, Color color);      // Draw text (default font) within an image (destination)
-- TODO void ImageDrawTextEx(Image *dst, Vector2 position, Font font, const char *text,
-- TODO                      float fontSize, float spacing, Color color);                                   // Draw text (custom sprite font) within an image (destination)
-- TODO void ImageFlipVertical(Image *image);                                                               // Flip image vertically
-- TODO void ImageFlipHorizontal(Image *image);                                                             // Flip image horizontally
-- TODO void ImageRotateCW(Image *image);                                                                   // Rotate image clockwise 90deg
-- TODO void ImageRotateCCW(Image *image);                                                                  // Rotate image counter-clockwise 90deg
-- TODO void ImageColorTint(Image *image, Color color);                                                     // Modify image color: tint
-- TODO void ImageColorInvert(Image *image);                                                                // Modify image color: invert
-- TODO void ImageColorGrayscale(Image *image);                                                             // Modify image color: grayscale
-- TODO void ImageColorContrast(Image *image, float contrast);                                              // Modify image color: contrast (-100 to 100)
-- TODO void ImageColorBrightness(Image *image, int brightness);                                            // Modify image color: brightness (-255 to 255)
-- TODO void ImageColorReplace(Image *image, Color color, Color replace);                                   // Modify image color: replace color

-- TODO // Image generation functions
-- TODO Image GenImageColor(int width, int height, Color color);                                            // Generate image: plain color
-- TODO Image GenImageGradientV(int width, int height, Color top, Color bottom);                            // Generate image: vertical gradient
-- TODO Image GenImageGradientH(int width, int height, Color left, Color right);                            // Generate image: horizontal gradient
-- TODO Image GenImageGradientRadial(int width, int height, float density, Color inner, Color outer);       // Generate image: radial gradient
-- TODO Image GenImageChecked(int width, int height, int checksX, int checksY, Color col1, Color col2);     // Generate image: checked
-- TODO Image GenImageWhiteNoise(int width, int height, float factor);                                      // Generate image: white noise
-- TODO Image GenImagePerlinNoise(int width, int height, int offsetX, int offsetY, float scale);            // Generate image: perlin noise
-- TODO Image GenImageCellular(int width, int height, int tileSize);                                        // Generate image: cellular algorithm. Bigger tileSize means bigger cells

-- TODO // Texture2D configuration functions
-- TODO void GenTextureMipmaps(Texture2D *texture);                                                         // Generate GPU mipmaps for a texture
-- TODO void SetTextureFilter(Texture2D texture, int filterMode);                                           // Set texture scaling filter mode
-- TODO void SetTextureWrap(Texture2D texture, int wrapMode);                                               // Set texture wrapping mode

-- TODO // Texture2D drawing functions
-- TODO void DrawTexture(Texture2D texture, int posX, int posY, Color tint);                                // Draw a Texture2D
-- TODO void DrawTextureV(Texture2D texture, Vector2 position, Color tint);                                 // Draw a Texture2D with position defined as Vector2
-- TODO void DrawTextureEx(Texture2D texture, Vector2 position, float rotation, float scale, Color tint);   // Draw a Texture2D with extended parameters
-- TODO void DrawTextureRec(Texture2D texture, Rectangle sourceRec, Vector2 position, Color tint);          // Draw a part of a texture defined by a rectangle
-- TODO void DrawTexturePro(Texture2D texture, Rectangle sourceRec, Rectangle destRec, Vector2 origin,      // Draw a part of a texture defined by a rectangle with 'pro' parameters
-- TODO                     float rotation, Color tint);

---------------------------------------------------------------------------------
-- Text
---------------------------------------------------------------------------------

{# fun unsafe WrappedGetFontDefault as getFontDefault
  {} -> `Font' #}

{# fun unsafe WrappedLoadFont as loadFont
  {`String'} -> `Font' #}

-- TODO // Font loading/unloading functions
-- TODO Font LoadFontEx(const char *fileName, int fontSize, int charsCount, int *fontChars);              // Load font from file with extended parameters
-- TODO CharInfo *LoadFontData(const char *fileName, int fontSize, int *fontChars, int charsCount, bool sdf); // Load font data for further use
-- TODO Image GenImageFontAtlas(CharInfo *chars, int fontSize, int charsCount, int padding, int packMethod);  // Generate image font atlas using chars info

{# fun unsafe DrawFPS as ^
  {`Int', `Int'} -> `()' #}

{# fun unsafe DrawText as ^
  {`String', `Int', `Int', `Int', with* %`Color'} -> `()' #}

{# fun unsafe DrawTextEx as ^
  {%`Font', `String', with* %`Vector2', `Float', `Float', with* %`Color'} -> `()' #}

-- TODO // Text misc. functions
-- TODO int MeasureText(const char *text, int fontSize);                                                  // Measure string width for default font
-- TODO Vector2 MeasureTextEx(Font font, const char *text, float fontSize, float spacing);                // Measure string size for Font
-- TODO const char *SubText(const char *text, int position, int length);                                  // Get a piece of a text string
-- TODO int GetGlyphIndex(Font font, int character);                                                      // Get index position for a unicode character on font
