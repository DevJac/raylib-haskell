{-# LANGUAGE ForeignFunctionInterface #-}
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

-- | Color r g b a
data Color = Color !Word8 !Word8 !Word8 !Word8

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

-- | Rectangle x y width height
data Rectangle = Rectangle !Float !Float !Float !Float

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

-- | Vector2 x y
data Vector2 = Vector2 !Float !Float

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

-- | Vector3 x y z
data Vector3 = Vector3 !Float !Float !Float deriving Show

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

-- | Vector4 x y z w
data Vector4 = Vector4 !Float !Float !Float !Float

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

-- | Matrix m0 m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 m13 m14 m15
data Matrix = Matrix !Float !Float !Float !Float
                     !Float !Float !Float !Float
                     !Float !Float !Float !Float
                     !Float !Float !Float !Float

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

-- | Camera3D position target up fovy type
data Camera3D = Camera3D !Vector3 !Vector3 !Vector3 !Float !Int deriving Show

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

-- | Camera2D offset target rotation zoom
data Camera2D = Camera2D !Vector2 !Vector2 !Float !Float

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

-- | Ray position direction
data Ray = Ray !Vector3 !Vector3

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

-- | RayHitInfo hit distance position normal
data RayHitInfo = RayHitInfo !Bool !Float !Vector3 !Vector3

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
