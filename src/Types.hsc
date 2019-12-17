{-# LANGUAGE ForeignFunctionInterface #-}
module Types (

  -- * Enum types
  ConfigFlag (
      FullscreenMode,
      WindowResizable,
      WindowUndecorated,
      WindowTransparent,
      Msaa4x,
      Vsync),
  LogType (Debug, Info, Warning, Error),
  CameraType (Perspective, Orthographic),
  CameraMode (Custom, Free, Orbital, FirstPerson, ThirdPerson),
  MouseButton (LeftClick, RightClick, MiddleClick),
  MaterialMapType (
      Albedo,
      Metalness,
      Normal,
      Roughness,
      Occlusion,
      Emission,
      Height,
      Cubemap,
      Irradiance,
      Prefilter,
      BRDF),

  -- * Simple types
  Color (Color),
  Rectangle (Rectangle),
  Vector2 (Vector2),
  Vector3 (Vector3),
  Camera3D (Camera3D),

  -- * Complex types
  Image (Image), withImage, imageWidth, imageHeight,
  Texture2D (Texture2D), withTexture2D, texture2DWidth, texture2DHeight,
  Font (Font), withFont, fontBaseSize, fontCharsCount,
  Mesh (Mesh), withMesh,
  MaterialMap (MaterialMap),
  Material (Material), withMaterial, materialSetMap,
  Model (Model), withModel, modelSetMeshes, modelSetMaterials,

  -- * Other types

) where
import Data.Foldable
import Data.IORef
import Data.Word
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable

#include "raylib.h"
#include "types.h"

data LogType = All
             | Trace
             | Debug
             | Info
             | Warning
             | Error
             | Fatal
             | None
             deriving (Show, Eq)

instance Enum LogType where
  fromEnum All     = #{const LOG_ALL}
  fromEnum Trace   = #{const LOG_TRACE}
  fromEnum Debug   = #{const LOG_DEBUG}
  fromEnum Info    = #{const LOG_INFO}
  fromEnum Warning = #{const LOG_WARNING}
  fromEnum Error   = #{const LOG_ERROR}
  fromEnum Fatal   = #{const LOG_FATAL}
  fromEnum None    = #{const LOG_NONE}
  -- I don't think raylib ever returns LogType from a function, thus we probably wont ever use toEnum.
  toEnum #{const LOG_ALL}     = All
  toEnum #{const LOG_TRACE}   = Trace
  toEnum #{const LOG_DEBUG}   = Debug
  toEnum #{const LOG_INFO}    = Info
  toEnum #{const LOG_WARNING} = Warning
  toEnum #{const LOG_ERROR}   = Error
  toEnum #{const LOG_FATAL}   = Fatal
  toEnum #{const LOG_NONE}    = None
  toEnum unknown              = error $ "Received an unknown LogType value from raylib: " ++ (show unknown)

data ConfigFlag = FullscreenMode
                | WindowResizable
                | WindowUndecorated
                | WindowTransparent
                | WindowHidden
                | WindowAlwaysRun
                | Msaa4x
                | Vsync
                deriving (Show, Eq)

instance Enum ConfigFlag where
  fromEnum FullscreenMode    = #{const FLAG_FULLSCREEN_MODE}
  fromEnum WindowResizable   = #{const FLAG_WINDOW_RESIZABLE}
  fromEnum WindowUndecorated = #{const FLAG_WINDOW_UNDECORATED}
  fromEnum WindowTransparent = #{const FLAG_WINDOW_TRANSPARENT}
  fromEnum WindowHidden      = #{const FLAG_WINDOW_HIDDEN}
  fromEnum WindowAlwaysRun   = #{const FLAG_WINDOW_ALWAYS_RUN}
  fromEnum Msaa4x            = #{const FLAG_MSAA_4X_HINT}
  fromEnum Vsync             = #{const FLAG_VSYNC_HINT}
  -- I don't think raylib ever returns ConfigFlags from a function, thus we probably wont ever use toEnum.
  toEnum #{const FLAG_FULLSCREEN_MODE}    = FullscreenMode
  toEnum #{const FLAG_WINDOW_RESIZABLE}   = WindowResizable
  toEnum #{const FLAG_WINDOW_UNDECORATED} = WindowUndecorated
  toEnum #{const FLAG_WINDOW_TRANSPARENT} = WindowTransparent
  toEnum #{const FLAG_WINDOW_HIDDEN}      = WindowHidden
  toEnum #{const FLAG_WINDOW_ALWAYS_RUN}  = WindowAlwaysRun
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

data MaterialMapType = Albedo
                     | Metalness
                     | Normal
                     | Roughness
                     | Occlusion
                     | Emission
                     | Height
                     | Cubemap
                     | Irradiance
                     | Prefilter
                     | BRDF
                     deriving (Show, Eq)

instance Enum MaterialMapType where
  fromEnum Albedo     = #{const MAP_ALBEDO}
  fromEnum Metalness  = #{const MAP_METALNESS}
  fromEnum Normal     = #{const MAP_NORMAL}
  fromEnum Roughness  = #{const MAP_ROUGHNESS}
  fromEnum Occlusion  = #{const MAP_OCCLUSION}
  fromEnum Emission   = #{const MAP_EMISSION}
  fromEnum Height     = #{const MAP_HEIGHT}
  fromEnum Cubemap    = #{const MAP_CUBEMAP}
  fromEnum Irradiance = #{const MAP_IRRADIANCE}
  fromEnum Prefilter  = #{const MAP_PREFILTER}
  fromEnum BRDF       = #{const MAP_BRDF}
  toEnum #{const MAP_ALBEDO}     = Albedo
  toEnum #{const MAP_METALNESS}  = Metalness
  toEnum #{const MAP_NORMAL}     = Normal
  toEnum #{const MAP_ROUGHNESS}  = Roughness
  toEnum #{const MAP_OCCLUSION}  = Occlusion
  toEnum #{const MAP_EMISSION}   = Emission
  toEnum #{const MAP_HEIGHT}     = Height
  toEnum #{const MAP_CUBEMAP}    = Cubemap
  toEnum #{const MAP_IRRADIANCE} = Irradiance
  toEnum #{const MAP_PREFILTER}  = Prefilter
  toEnum #{const MAP_BRDF}       = BRDF
  toEnum unknown                 = error $ "Received an unknown MaterialMapType value from raylib: " ++ (show unknown)

-- | Like Storable, but without peek, only poke.
class Pokable a where
  pokableSizeOf :: a -> Int
  pokablePoke :: Ptr a -> a -> IO ()

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

data MaterialMap = MaterialMap !Texture2D !Color !Float deriving Show

instance Pokable MaterialMap where
  pokableSizeOf _ = #{size MaterialMap}
  pokablePoke p (MaterialMap texture color value) = do
    let texturePtr = #{ptr MaterialMap, texture} p
        colorPtr   = #{ptr MaterialMap, color}   p
        valuePtr   = #{ptr MaterialMap, value}   p
    pokablePoke texturePtr texture
    poke colorPtr color
    poke valuePtr value

withMaterialMap :: MaterialMap -> (Ptr MaterialMap -> IO a) -> IO a
withMaterialMap materialMap f =
  allocaBytes (pokableSizeOf materialMap) $ \materialMapPtr -> do
    pokablePoke materialMapPtr materialMap
    f materialMapPtr

newtype Font = Font (ForeignPtr Font) deriving Show

withFont :: Font -> (Ptr Font -> IO a) -> IO a
withFont (Font fontForeignPtr) f = withForeignPtr fontForeignPtr f

fontBaseSize :: Font -> IO Int
fontBaseSize font =
  withFont font $ \fontPtr ->
    fromIntegral <$> (#{peek Font, baseSize} fontPtr :: IO CInt)

fontCharsCount :: Font -> IO Int
fontCharsCount font =
  withFont font $ \fontPtr ->
    fromIntegral <$> (#{peek Font, charsCount} fontPtr :: IO CInt)

newtype Image = Image (ForeignPtr Image) deriving Show

withImage :: Image -> (Ptr Image -> IO a) -> IO a
withImage (Image imageForeignPtr) f = withForeignPtr imageForeignPtr f

imageWidth :: Image -> IO Int
imageWidth image =
  withImage image $ \imagePtr ->
    fromIntegral <$> (#{peek Image, width} imagePtr :: IO CInt)

imageHeight :: Image -> IO Int
imageHeight image =
  withImage image $ \imagePtr ->
    fromIntegral <$> (#{peek Image, height} imagePtr :: IO CInt)

newtype Texture2D = Texture2D (ForeignPtr Texture2D) deriving Show

instance Pokable Texture2D where
  pokableSizeOf _ = #{size Texture2D}
  pokablePoke p texture =
    withTexture2D texture $ \texturePtr ->
      copyBytes p texturePtr (pokableSizeOf texture)

withTexture2D :: Texture2D -> (Ptr Texture2D -> IO a) -> IO a
withTexture2D (Texture2D texture2DForeignPtr) f = withForeignPtr texture2DForeignPtr f

texture2DWidth :: Texture2D -> IO Int
texture2DWidth texture =
  withTexture2D texture $ \texturePtr ->
    fromIntegral <$> (#{peek Texture2D, width} texturePtr :: IO CInt)

texture2DHeight :: Texture2D -> IO Int
texture2DHeight texture =
  withTexture2D texture $ \texturePtr ->
    fromIntegral <$> (#{peek Texture2D, height} texturePtr :: IO CInt)

data Material = Material (ForeignPtr Material)

instance Pokable Material where
  pokableSizeOf _ = #{size Material}
  pokablePoke p material =
    withMaterial material $ \materialPtr ->
      copyBytes p materialPtr (pokableSizeOf material)

withMaterial :: Material -> (Ptr Material -> IO a) -> IO a
withMaterial (Material materialForeignPtr) f = withForeignPtr materialForeignPtr f

foreign import ccall unsafe "types.h MaterialSetMap" c_MaterialSetMap :: Ptr Material -> CInt -> Ptr MaterialMap -> IO ()
materialSetMap :: Material -> MaterialMapType -> MaterialMap -> IO ()
materialSetMap material mapType materialMap =
  withMaterial material $ \materialPtr ->
    withMaterialMap materialMap $ \materialMapPtr -> do
      c_MaterialSetMap materialPtr (fromIntegral (fromEnum mapType)) materialMapPtr

newtype Model = Model (ForeignPtr Model)

withModel :: Model -> (Ptr Model -> IO a) -> IO a
withModel (Model modelForeignPtr) f = withForeignPtr modelForeignPtr f

foreign import ccall unsafe "types.h ModelSetMeshes" c_ModelSetMeshes :: Ptr Model -> Ptr Mesh -> CInt -> IO ()
modelSetMeshes :: Model -> [Mesh] -> IO ()
modelSetMeshes model meshes =
  withModel model $ \modelPtr ->
    allocaBytes (length meshes * #{size Mesh}) $ \meshesArrayPtr -> do
      for_ (zip [0..] meshes) $ \(i, mesh) -> do
        let p = meshesArrayPtr `plusPtr` (i * pokableSizeOf mesh)
        pokablePoke p mesh
      c_ModelSetMeshes modelPtr meshesArrayPtr (fromIntegral (length meshes))

foreign import ccall unsafe "types.h ModelSetMaterials" c_ModelSetMaterials :: Ptr Model -> Ptr Material -> CInt -> IO ()
modelSetMaterials :: Model -> [Material] -> IO ()
modelSetMaterials model materials =
  withModel model $ \modelPtr ->
    allocaBytes (length materials * #{size Material}) $ \materialsArrayPtr -> do
      for_ (zip [0..] materials) $ \(i, material) -> do
        let p = materialsArrayPtr `plusPtr` (i * pokableSizeOf material)
        pokablePoke p material
      c_ModelSetMaterials modelPtr materialsArrayPtr (fromIntegral (length materials))

newtype Mesh = Mesh (ForeignPtr Mesh)

instance Pokable Mesh where
  pokableSizeOf _ = #{size Mesh}
  pokablePoke p mesh =
    withMesh mesh $ \meshPtr ->
      copyBytes p meshPtr (pokableSizeOf mesh)

withMesh :: Mesh -> (Ptr Mesh -> IO a) -> IO a
withMesh (Mesh meshForeignPtr) f = withForeignPtr meshForeignPtr f
