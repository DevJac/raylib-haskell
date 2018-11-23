{-# LANGUAGE ForeignFunctionInterface #-}
module Types (

  -- * Enum types
  ConfigFlag (
      ShowLogo,
      FullscreenMode,
      WindowResizable,
      WindowUndecorated,
      WindowTransparent,
      Msaa4x,
      Vsync),
  LogType (Debug, Info, Warning, Error),
  CameraType (Perspective, Orthographic),
  CameraMode (Custom, Free, Orbital, FirstPerson, ThirdPerson),
  KeyboardKey (..),
  MouseButton (LeftClick, RightClick, MiddleClick),

  -- * Simple types
  Color (Color),
  Rectangle (Rectangle),
  Vector2 (Vector2),
  Vector3 (Vector3),
  Camera3D (Camera3D),

  -- * Complex types
  Image (Image), imageWidth, imageHeight,
  Font (Font), fontBaseSize, fontCharsCount,
  Texture2D (Texture2D),

  -- * Other types

) where
import Data.Word
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.ForeignPtr
import Foreign.Storable

#include "raylib.h"

data LogType = Info
             | Warning
             | Error
             | Debug
             | Other
             deriving (Show, Eq)

instance Enum LogType where
  fromEnum Info    = #{const LOG_INFO}
  fromEnum Warning = #{const LOG_WARNING}
  fromEnum Error   = #{const LOG_ERROR}
  fromEnum Debug   = #{const LOG_DEBUG}
  fromEnum Other   = #{const LOG_OTHER}
  -- I don't think raylib ever returns LogType from a function, thus we probably wont ever use toEnum.
  toEnum #{const LOG_INFO}    = Info
  toEnum #{const LOG_WARNING} = Warning
  toEnum #{const LOG_ERROR}   = Error
  toEnum #{const LOG_DEBUG}   = Debug
  toEnum #{const LOG_OTHER}   = Other
  toEnum unknown              = error $ "Received an unknown LogType value from raylib: " ++ (show unknown)

data ConfigFlag = ShowLogo
                | FullscreenMode
                | WindowResizable
                | WindowUndecorated
                | WindowTransparent
                | Msaa4x
                | Vsync
                deriving (Show, Eq)

instance Enum ConfigFlag where
  fromEnum ShowLogo          = #{const FLAG_SHOW_LOGO}
  fromEnum FullscreenMode    = #{const FLAG_FULLSCREEN_MODE}
  fromEnum WindowResizable   = #{const FLAG_WINDOW_RESIZABLE}
  fromEnum WindowUndecorated = #{const FLAG_WINDOW_UNDECORATED}
  fromEnum WindowTransparent = #{const FLAG_WINDOW_TRANSPARENT}
  fromEnum Msaa4x            = #{const FLAG_MSAA_4X_HINT}
  fromEnum Vsync             = #{const FLAG_VSYNC_HINT}
  -- I don't think raylib ever returns ConfigFlags from a function, thus we probably wont ever use toEnum.
  toEnum #{const FLAG_SHOW_LOGO}          = ShowLogo
  toEnum #{const FLAG_FULLSCREEN_MODE}    = FullscreenMode
  toEnum #{const FLAG_WINDOW_RESIZABLE}   = WindowResizable
  toEnum #{const FLAG_WINDOW_UNDECORATED} = WindowUndecorated
  toEnum #{const FLAG_WINDOW_TRANSPARENT} = WindowTransparent
  toEnum #{const FLAG_MSAA_4X_HINT}       = Msaa4x
  toEnum #{const FLAG_VSYNC_HINT}         = Vsync
  toEnum unknown                          = error $ "Received an unknown ConfigFlag value from raylib: " ++ (show unknown)

data CameraType = Perspective
                | Orthographic
                deriving (Show, Eq)

instance Enum CameraType where
  fromEnum Perspective  = #{const CAMERA_PERSPECTIVE}
  fromEnum Orthographic = #{const CAMERA_ORTHOGRAPHIC}
  toEnum #{const CAMERA_PERSPECTIVE}  = Perspective
  toEnum #{const CAMERA_ORTHOGRAPHIC} = Orthographic
  toEnum unknown                      = error $ "Received an unknown CameraType value from raylib: " ++ (show unknown)

data CameraMode = Custom
                | Free
                | Orbital
                | FirstPerson
                | ThirdPerson
                deriving (Show, Eq)

instance Enum CameraMode where
  fromEnum Custom      = #{const CAMERA_CUSTOM}
  fromEnum Free        = #{const CAMERA_FREE}
  fromEnum Orbital     = #{const CAMERA_ORBITAL}
  fromEnum FirstPerson = #{const CAMERA_FIRST_PERSON}
  fromEnum ThirdPerson = #{const CAMERA_THIRD_PERSON}
  toEnum #{const CAMERA_CUSTOM}       = Custom
  toEnum #{const CAMERA_FREE}         = Free
  toEnum #{const CAMERA_ORBITAL}      = Orbital
  toEnum #{const CAMERA_FIRST_PERSON} = FirstPerson
  toEnum #{const CAMERA_THIRD_PERSON} = ThirdPerson
  toEnum unknown                      = error $ "Received an unknown CameraMode value from raylib: " ++ (show unknown)

data KeyboardKey = Space
                 | Escape
                 | Enter
                 | Tab
                 | Backspace
                 | Insert
                 | Delete
                 | RightArrow
                 | LeftArrow
                 | DownArrow
                 | UpArrow
                 | PageUp
                 | PageDown
                 | Home
                 | End
                 | CapsLock
                 | ScrollLock
                 | NumLock
                 | PrintScreen
                 | Pause
                 | F1
                 | F2
                 | F3
                 | F4
                 | F5
                 | F6
                 | F7
                 | F8
                 | F9
                 | F10
                 | F11
                 | F12
                 | LeftShift
                 | LeftControl
                 | LeftAlt
                 | RightShift
                 | RightControl
                 | RightAlt
                 | Grave
                 | Slash
                 | Backslash
                 | Zero
                 | One
                 | Two
                 | Three
                 | Four
                 | Five
                 | Six
                 | Seven
                 | Eight
                 | Nine
                 | A
                 | B
                 | C
                 | D
                 | E
                 | F
                 | G
                 | H
                 | I
                 | J
                 | K
                 | L
                 | M
                 | N
                 | O
                 | P
                 | Q
                 | R
                 | S
                 | T
                 | U
                 | V
                 | W
                 | X
                 | Y
                 | Z
                 | NoKey
                 | OtherKey Int
                 deriving (Show, Eq)

instance Enum KeyboardKey where
  fromEnum Space        = #{const KEY_SPACE}
  fromEnum Escape       = #{const KEY_ESCAPE}
  fromEnum Enter        = #{const KEY_ENTER}
  fromEnum Tab          = #{const KEY_TAB}
  fromEnum Backspace    = #{const KEY_BACKSPACE}
  fromEnum Insert       = #{const KEY_INSERT}
  fromEnum Delete       = #{const KEY_DELETE}
  fromEnum RightArrow   = #{const KEY_RIGHT}
  fromEnum LeftArrow    = #{const KEY_LEFT}
  fromEnum DownArrow    = #{const KEY_DOWN}
  fromEnum UpArrow      = #{const KEY_UP}
  fromEnum PageUp       = #{const KEY_PAGE_UP}
  fromEnum PageDown     = #{const KEY_PAGE_DOWN}
  fromEnum Home         = #{const KEY_HOME}
  fromEnum End          = #{const KEY_END}
  fromEnum CapsLock     = #{const KEY_CAPS_LOCK}
  fromEnum ScrollLock   = #{const KEY_SCROLL_LOCK}
  fromEnum NumLock      = #{const KEY_NUM_LOCK}
  fromEnum PrintScreen  = #{const KEY_PRINT_SCREEN}
  fromEnum Pause        = #{const KEY_PAUSE}
  fromEnum F1           = #{const KEY_F1}
  fromEnum F2           = #{const KEY_F2}
  fromEnum F3           = #{const KEY_F3}
  fromEnum F4           = #{const KEY_F4}
  fromEnum F5           = #{const KEY_F5}
  fromEnum F6           = #{const KEY_F6}
  fromEnum F7           = #{const KEY_F7}
  fromEnum F8           = #{const KEY_F8}
  fromEnum F9           = #{const KEY_F9}
  fromEnum F10          = #{const KEY_F10}
  fromEnum F11          = #{const KEY_F11}
  fromEnum F12          = #{const KEY_F12}
  fromEnum LeftShift    = #{const KEY_LEFT_SHIFT}
  fromEnum LeftControl  = #{const KEY_LEFT_CONTROL}
  fromEnum LeftAlt      = #{const KEY_LEFT_ALT}
  fromEnum RightShift   = #{const KEY_RIGHT_SHIFT}
  fromEnum RightControl = #{const KEY_RIGHT_CONTROL}
  fromEnum RightAlt     = #{const KEY_RIGHT_ALT}
  fromEnum Grave        = #{const KEY_GRAVE}
  fromEnum Slash        = #{const KEY_SLASH}
  fromEnum Backslash    = #{const KEY_BACKSLASH}
  fromEnum Zero         = #{const KEY_ZERO}
  fromEnum One          = #{const KEY_ONE}
  fromEnum Two          = #{const KEY_TWO}
  fromEnum Three        = #{const KEY_THREE}
  fromEnum Four         = #{const KEY_FOUR}
  fromEnum Five         = #{const KEY_FIVE}
  fromEnum Six          = #{const KEY_SIX}
  fromEnum Seven        = #{const KEY_SEVEN}
  fromEnum Eight        = #{const KEY_EIGHT}
  fromEnum Nine         = #{const KEY_NINE}
  fromEnum A            = #{const KEY_A}
  fromEnum B            = #{const KEY_B}
  fromEnum C            = #{const KEY_C}
  fromEnum D            = #{const KEY_D}
  fromEnum E            = #{const KEY_E}
  fromEnum F            = #{const KEY_F}
  fromEnum G            = #{const KEY_G}
  fromEnum H            = #{const KEY_H}
  fromEnum I            = #{const KEY_I}
  fromEnum J            = #{const KEY_J}
  fromEnum K            = #{const KEY_K}
  fromEnum L            = #{const KEY_L}
  fromEnum M            = #{const KEY_M}
  fromEnum N            = #{const KEY_N}
  fromEnum O            = #{const KEY_O}
  fromEnum P            = #{const KEY_P}
  fromEnum Q            = #{const KEY_Q}
  fromEnum R            = #{const KEY_R}
  fromEnum S            = #{const KEY_S}
  fromEnum T            = #{const KEY_T}
  fromEnum U            = #{const KEY_U}
  fromEnum V            = #{const KEY_V}
  fromEnum W            = #{const KEY_W}
  fromEnum X            = #{const KEY_X}
  fromEnum Y            = #{const KEY_Y}
  fromEnum Z            = #{const KEY_Z}
  fromEnum NoKey        = (-1)
  fromEnum (OtherKey i) = i
  toEnum #{const KEY_SPACE}         = Space
  toEnum #{const KEY_ESCAPE}        = Escape
  toEnum #{const KEY_ENTER}         = Enter
  toEnum #{const KEY_TAB}           = Tab
  toEnum #{const KEY_BACKSPACE}     = Backspace
  toEnum #{const KEY_INSERT}        = Insert
  toEnum #{const KEY_DELETE}        = Delete
  toEnum #{const KEY_RIGHT}         = RightArrow
  toEnum #{const KEY_LEFT}          = LeftArrow
  toEnum #{const KEY_DOWN}          = DownArrow
  toEnum #{const KEY_UP}            = UpArrow
  toEnum #{const KEY_PAGE_UP}       = PageUp
  toEnum #{const KEY_PAGE_DOWN}     = PageDown
  toEnum #{const KEY_HOME}          = Home
  toEnum #{const KEY_END}           = End
  toEnum #{const KEY_CAPS_LOCK}     = CapsLock
  toEnum #{const KEY_SCROLL_LOCK}   = ScrollLock
  toEnum #{const KEY_NUM_LOCK}      = NumLock
  toEnum #{const KEY_PRINT_SCREEN}  = PrintScreen
  toEnum #{const KEY_PAUSE}         = Pause
  toEnum #{const KEY_F1}            = F1
  toEnum #{const KEY_F2}            = F2
  toEnum #{const KEY_F3}            = F3
  toEnum #{const KEY_F4}            = F4
  toEnum #{const KEY_F5}            = F5
  toEnum #{const KEY_F6}            = F6
  toEnum #{const KEY_F7}            = F7
  toEnum #{const KEY_F8}            = F8
  toEnum #{const KEY_F9}            = F9
  toEnum #{const KEY_F10}           = F10
  toEnum #{const KEY_F11}           = F11
  toEnum #{const KEY_F12}           = F12
  toEnum #{const KEY_LEFT_SHIFT}    = LeftShift
  toEnum #{const KEY_LEFT_CONTROL}  = LeftControl
  toEnum #{const KEY_LEFT_ALT}      = LeftAlt
  toEnum #{const KEY_RIGHT_SHIFT}   = RightShift
  toEnum #{const KEY_RIGHT_CONTROL} = RightControl
  toEnum #{const KEY_RIGHT_ALT}      = RightAlt
  toEnum #{const KEY_GRAVE}         = Grave
  toEnum #{const KEY_SLASH}         = Slash
  toEnum #{const KEY_BACKSLASH}     = Backslash
  toEnum #{const KEY_ZERO}          = Zero
  toEnum #{const KEY_ONE}           = One
  toEnum #{const KEY_TWO}           = Two
  toEnum #{const KEY_THREE}         = Three
  toEnum #{const KEY_FOUR}          = Four
  toEnum #{const KEY_FIVE}          = Five
  toEnum #{const KEY_SIX}           = Six
  toEnum #{const KEY_SEVEN}         = Seven
  toEnum #{const KEY_EIGHT}         = Eight
  toEnum #{const KEY_NINE}          = Nine
  toEnum #{const KEY_A}             = A
  toEnum #{const KEY_B}             = B
  toEnum #{const KEY_C}             = C
  toEnum #{const KEY_D}             = D
  toEnum #{const KEY_E}             = E
  toEnum #{const KEY_F}             = F
  toEnum #{const KEY_G}             = G
  toEnum #{const KEY_H}             = H
  toEnum #{const KEY_I}             = I
  toEnum #{const KEY_J}             = J
  toEnum #{const KEY_K}             = K
  toEnum #{const KEY_L}             = L
  toEnum #{const KEY_M}             = M
  toEnum #{const KEY_N}             = N
  toEnum #{const KEY_O}             = O
  toEnum #{const KEY_P}             = P
  toEnum #{const KEY_Q}             = Q
  toEnum #{const KEY_R}             = R
  toEnum #{const KEY_S}             = S
  toEnum #{const KEY_T}             = T
  toEnum #{const KEY_U}             = U
  toEnum #{const KEY_V}             = V
  toEnum #{const KEY_W}             = W
  toEnum #{const KEY_X}             = X
  toEnum #{const KEY_Y}             = Y
  toEnum #{const KEY_Z}             = Z
  toEnum (-1)                       = NoKey
  toEnum i                          = OtherKey i

-- The word "click" seems to uniquely apply to the mouse, so it's a good word to use in our data constructors.
data MouseButton = LeftClick
                 | RightClick
                 | MiddleClick
                 | OtherClick Int
                 deriving (Show, Eq)

instance Enum MouseButton where
  fromEnum LeftClick      = #{const MOUSE_LEFT_BUTTON}
  fromEnum RightClick     = #{const MOUSE_RIGHT_BUTTON}
  fromEnum MiddleClick    = #{const MOUSE_MIDDLE_BUTTON}
  fromEnum (OtherClick i) = i
  toEnum #{const MOUSE_LEFT_BUTTON}   = LeftClick
  toEnum #{const MOUSE_RIGHT_BUTTON}  = RightClick
  toEnum #{const MOUSE_MIDDLE_BUTTON} = MiddleClick
  toEnum i                            = OtherClick i

-- | @Color r g b a@
data Color = Color !Word8 !Word8 !Word8 !Word8 deriving (Show, Eq)

instance Storable Color where
  sizeOf _ = #{size Color}
  alignment _ = #{alignment Color}
  peek p = do
    r <- #{peek Color, r} p
    g <- #{peek Color, g} p
    b <- #{peek Color, b} p
    a <- #{peek Color, a} p
    pure $ Color r g b a
  poke p (Color r g b a) = do
    #{poke Color, r} p r
    #{poke Color, g} p g
    #{poke Color, b} p b
    #{poke Color, a} p a

-- | @Rectangle x y width height@
data Rectangle = Rectangle !Float !Float !Float !Float deriving (Show, Eq)

instance Storable Rectangle where
  sizeOf _ = #{size Rectangle}
  alignment _ = #{alignment Rectangle}
  peek p = do
    x <- #{peek Rectangle, x} p
    y <- #{peek Rectangle, y} p
    width <- #{peek Rectangle, width} p
    height <- #{peek Rectangle, height} p
    pure $ Rectangle x y width height
  poke p (Rectangle x y width height) = do
    #{poke Rectangle, x} p x
    #{poke Rectangle, y} p y
    #{poke Rectangle, width} p width
    #{poke Rectangle, height} p height

-- | @Vector2 x y@
data Vector2 = Vector2 !Float !Float deriving (Show, Eq)

instance Storable Vector2 where
  sizeOf _ = #{size Vector2}
  alignment _ = #{alignment Vector2}
  peek p = do
    x <- #{peek Vector2, x} p
    y <- #{peek Vector2, y} p
    pure $ Vector2 x y
  poke p (Vector2 x y) = do
    #{poke Vector2, x} p x
    #{poke Vector2, y} p y

data Vector3 = Vector3 !Float !Float !Float deriving (Show, Eq)

instance Storable Vector3 where
  sizeOf _ = #{size Vector3}
  alignment _ = #{alignment Vector3}
  peek p = do
    x <- #{peek Vector3, x} p
    y <- #{peek Vector3, y} p
    z <- #{peek Vector3, z} p
    pure $ Vector3 x y z
  poke p (Vector3 x y z) = do
    #{poke Vector3, x} p x
    #{poke Vector3, y} p y
    #{poke Vector3, z} p z

data Camera3D = Camera3D !Vector3 !Vector3 !Vector3 !Float !CameraType deriving (Show, Eq)

instance Storable Camera3D where
  sizeOf _ = #{size Camera3D}
  alignment _ = #{alignment Camera3D}
  peek p = do
    position <- #{peek Camera3D, position} p
    target   <- #{peek Camera3D, target}   p
    up       <- #{peek Camera3D, up}       p
    fovy     <- #{peek Camera3D, fovy}     p
    type_    <- #{peek Camera3D, type}     p
    pure $ Camera3D position target up fovy (toEnum type_)
  poke p (Camera3D position target up fovy type_) = do
    #{poke Camera3D, position} p position
    #{poke Camera3D, target}   p target
    #{poke Camera3D, up}       p up
    #{poke Camera3D, fovy}     p fovy
    #{poke Camera3D, type}     p (fromEnum type_)

data MaterialMap = MaterialMap !Texture2D !Color !Float deriving (Show)

instance Storable MaterialMap where
  sizeOf _ = #{size MaterialMap}
  alignment _ = #{alignment MaterialMap}
  peek p = do
    texture <- #{peek MaterialMap, texture} p
    color   <- #{peek MaterialMap, color}   p
    value   <- #{peek MaterialMap, value}   p
    pure $ MaterialMap texture color value
  poke p (MaterialMap texture color value) = do
    #{poke MaterialMap, texture} p texture
    #{poke MaterialMap, color}   p color
    #{poke MaterialMap, value}   p value

newtype Font = Font (ForeignPtr Font) deriving (Show)

fontBaseSize :: Font -> IO Int
fontBaseSize (Font fontForeignPtr) =
  withForeignPtr fontForeignPtr $ \fontPtr ->
    fromIntegral <$> (#{peek Font, baseSize} fontPtr :: IO CInt)

fontCharsCount :: Font -> IO Int
fontCharsCount (Font fontForeignPtr) =
  withForeignPtr fontForeignPtr $ \fontPtr ->
    fromIntegral <$> (#{peek Font, charsCount} fontPtr :: IO CInt)

newtype Image = Image (ForeignPtr Image) deriving (Show)

imageWidth :: Image -> IO Int
imageWidth (Image imageForeignPtr) =
  withForeignPtr imageForeignPtr $ \imagePtr ->
    fromIntegral <$> (#{peek Image, width} imagePtr :: IO CInt)

imageHeight :: Image -> IO Int
imageHeight (Image imageForeignPtr) =
  withForeignPtr imageForeignPtr $ \imagePtr ->
    fromIntegral <$> (#{peek Image, height} imagePtr :: IO CInt)

newtype Texture2D = Texture2D (ForeignPtr Texture2D) deriving (Show)

instance Storable Texture2D where
  sizeOf _ = #{size Texture2D}
  alignment _ = #{alignment Texture2D}
  peek originPtr = do
    copyPtr <- mallocBytes #{size Texture2D}
    copyBytes copyPtr originPtr #{size Texture2D}
    Texture2D <$> newForeignPtr finalizerFree copyPtr
  poke p (Texture2D textureForeignPtr) =
    withForeignPtr textureForeignPtr $ \texturePtr ->
      moveBytes p texturePtr #{size Texture2D}
