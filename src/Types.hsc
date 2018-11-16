module Types (

  -- * Enum types
  LogType (Info, Warning, Error, Debug, Other),

  -- * Simple types
  Color (Color),
  Rectangle (Rectangle),
  Vector2 (Vector2),

  -- * Complex types
  Image (Image),
  Font (Font),

  -- * Other types

) where
import Data.Word
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
  toEnum #{const LOG_INFO}    = Info
  toEnum #{const LOG_WARNING} = Warning
  toEnum #{const LOG_ERROR}   = Error
  toEnum #{const LOG_DEBUG}   = Debug
  toEnum #{const LOG_OTHER}   = Other
  toEnum unknown              = error $ "Received an unknown LogType value from raylib: " ++ (show unknown)

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

newtype Font = Font (ForeignPtr Font)

newtype Image = Image (ForeignPtr Image)
