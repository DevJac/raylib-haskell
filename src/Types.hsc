module Types (

  -- * Enum types

  -- * Simple types
  Color(Color),
  Rectangle(Rectangle),

  -- * Complex types

  -- * Other types

) where
import Data.Word
import Foreign.Storable

#include "raylib.h"

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
