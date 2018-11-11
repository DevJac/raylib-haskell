{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE InstanceSigs #-}
module Internal.Types where
import Data.Coerce
import Data.Word
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

#include "raylib.h"
#include "Types.h"

withWrapper :: Storable b => (Ptr a -> Ptr b -> IO ()) -> Ptr a -> (Ptr b -> IO c) -> IO c
withWrapper wf p f = do
  alloca $ \wrp -> do
    wf p wrp
    f wrp

-- | Color r g b a
data Color = Color Word8 Word8 Word8 Word8

{# pointer *Color as ColorPtr -> Color #}

instance Storable Color where
  sizeOf = const {# sizeof Color #}
  alignment = const {# alignof Color #}
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

-- | Rectangle x y width height
data Rectangle = Rectangle Float Float Float Float

{# pointer *Rectangle as RectanglePtr -> Rectangle #}

instance Storable Rectangle where
  sizeOf = const {# sizeof Rectangle #}
  alignment = const {# alignof Rectangle #}
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

-- | Vector2 x y
data Vector2 = Vector2 Float Float

{# pointer *Vector2 as Vector2Ptr -> Vector2 #}

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

-- | Vector3 x y z
data Vector3 = Vector3 Float Float Float deriving Show

{# pointer *Vector3 as Vector3Ptr -> Vector3 #}

instance Storable Vector3 where
  sizeOf = const {# sizeof Vector3 #}
  alignment = const {# alignof Vector3 #}
  peek p = do
    x <- realToFrac <$> {# get Vector3.x #} p
    y <- realToFrac <$> {# get Vector3.y #} p
    z <- realToFrac <$> {# get Vector3.z #} p
    pure $ Vector3 x y z
  poke p (Vector3 x y z) = do
    {# set Vector3.x #} p (realToFrac x)
    {# set Vector3.y #} p (realToFrac y)
    {# set Vector3.z #} p (realToFrac z)

-- | Vector4 x y z w
data Vector4 = Vector4 Float Float Float Float

{# pointer *Vector4 as Vector4Ptr -> Vector4 #}

instance Storable Vector4 where
  sizeOf = const {# sizeof Vector4 #}
  alignment = const {# alignof Vector4 #}
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

-- | Matrix m0 m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 m13 m14 m15
data Matrix = Matrix Float Float Float Float
                     Float Float Float Float
                     Float Float Float Float
                     Float Float Float Float

{# pointer *Matrix as MatrixPtr -> Matrix #}

instance Storable Matrix where
  sizeOf = const {# sizeof Matrix #}
  alignment = const {# alignof Matrix #}
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

data WrCamera3D = WrCamera3D Vector3Ptr Vector3Ptr Vector3Ptr Float Int

{# pointer *WrCamera3D as WrCamera3DPtr -> WrCamera3D #}

instance Storable WrCamera3D where
  sizeOf = const {# sizeof WrCamera3D #}
  alignment = const {# alignof WrCamera3D #}
  peek p = do
    position <- {# get WrCamera3D.position #} p
    target   <- {# get WrCamera3D.target #}   p
    up       <- {# get WrCamera3D.up #}       p
    fovy     <- realToFrac   <$> {# get WrCamera3D.fovy #} p
    type_    <- fromIntegral <$> {# get WrCamera3D.type #} p
    pure $ WrCamera3D position target up fovy type_
  poke p (WrCamera3D position target up fovy type_) = do
    {# set WrCamera3D.position #} p position
    {# set WrCamera3D.target #}   p target
    {# set WrCamera3D.up #}       p up
    {# set WrCamera3D.fovy #}     p (realToFrac fovy)
    {# set WrCamera3D.type #}     p (fromIntegral type_)

-- | Camera3D position target up fovy type
data Camera3D = Camera3D Vector3 Vector3 Vector3 Float Int deriving Show

{# pointer *Camera3D as Camera3DPtr -> Camera3D #}

{# fun unsafe WrapCamera3D as ^
  {%`Camera3DPtr', `WrCamera3DPtr'} -> `()' #}

{# fun unsafe UnwrapToCamera3D as ^
  {%`WrCamera3DPtr', `Camera3DPtr'} -> `()' #}

instance Storable Camera3D where
  sizeOf = const {# sizeof Camera3D #}
  alignment = const {# alignof Camera3D #}
  peek :: Camera3DPtr -> IO Camera3D
  peek p =
    withWrapper wrapCamera3D p $ \wrp -> peek wrp >>= \(WrCamera3D wrPosition wrTarget wrUp wrFovy wrType) -> do
      position <- peek wrPosition
      target   <- peek wrTarget
      up       <- peek wrUp
      pure $ Camera3D position target up wrFovy wrType
  poke :: Camera3DPtr -> Camera3D -> IO ()
  poke p (Camera3D position target up fovy type_) =
    withWrapper wrapCamera3D p $ \wrp -> do
      peek wrp >>= \(WrCamera3D wrPosition wrTarget wrUp _wrFovy _wrType) -> do
        poke wrPosition position
        poke wrTarget   target
        poke wrUp       up
      unwrapToCamera3D wrp p
      {# set Camera3D.fovy #} p (realToFrac fovy)
      {# set Camera3D.type #} p (fromIntegral type_)
