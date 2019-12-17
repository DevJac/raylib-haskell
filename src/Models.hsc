{-# LANGUAGE ForeignFunctionInterface #-}
module Models (

  -- * Basic geometric 3D shapes
  -- TODO drawLine3D,
  -- TODO drawCircle3D,
  drawCube,
  -- TODO drawCubeV,
  drawCubeWires,
  -- TODO drawCubeTexture,
  -- TODO drawSphere,
  -- TODO drawSphereEx,
  -- TODO drawSphereWires,
  -- TODO drawCylinder,
  -- TODO drawCylinderWires,
  -- TODO drawPlane,
  -- TODO drawRay,
  drawGrid,
  -- TODO drawGizmo,

  -- * Model loading functions
  -- TODO loadModel,
  loadModelFromMesh,

  -- * Mesh loading functions
  -- TODO loadMesh,
  -- TODO exportMesh,

  -- * Mesh manipulation functions
  -- TODO meshBoundingBox,
  -- TODO meshTangents,
  -- TODO meshBinormals,

  -- * Mesh generation functions
  -- TODO genMeshPlane,
  genMeshCube,
  -- TODO genMeshSphere,
  -- TODO genMeshHemiSphere,
  -- TODO genMeshCylinder,
  -- TODO genMeshTorus,
  -- TODO genMeshKnot,
  -- TODO genMeshHeightmap,
  genMeshCubicmap,

  -- * Material loading functions
  loadMaterials,
  loadMaterialDefault,

  -- * Model drawing functions
  drawModel,
  -- TODO drawModelEx,
  -- TODO drawModelWires,
  -- TODO drawModelWiresEx,
  -- TODO drawBoundingBox,
  drawBillboard,
  -- TODO drawBillboardRec,

  -- * Collision detection functions
  -- TODO checkCollisionSpheres,
  -- TODO checkCollisionBoxes,
  -- TODO checkCollisionBoxSphere,
  -- TODO checkCollisionRaySphere,
  -- TODO checkCollisionRaySphereEx,
  -- TODO checkCollisionRayBox,
  -- TODO getCollisionRayModel,
  -- TODO getCollisionRayTriangle,
  -- TODO getCollisionRayGround,

) where
import Data.IORef
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Utils
import Foreign.Ptr
import Types

#include "raylib.h"
#include "models.h"

foreign import ccall unsafe "models.h WrappedDrawCube" c_WrappedDrawCube :: Ptr Vector3 -> CFloat -> CFloat -> CFloat -> Ptr Color -> IO ()
drawCube :: Vector3 -> Float -> Float -> Float -> Color -> IO ()
drawCube position width height length_ color =
  with position $ \positionPtr ->
    with color $ \colorPtr ->
      c_WrappedDrawCube positionPtr (realToFrac width) (realToFrac height) (realToFrac length_) colorPtr

foreign import ccall unsafe "models.h WrappedDrawCubeWires" c_WrappedDrawCubeWires :: Ptr Vector3 -> CFloat -> CFloat -> CFloat -> Ptr Color -> IO ()
drawCubeWires :: Vector3 -> Float -> Float -> Float -> Color -> IO ()
drawCubeWires position width height length_ color =
  with position $ \positionPtr ->
    with color $ \colorPtr ->
      c_WrappedDrawCubeWires positionPtr (realToFrac width) (realToFrac height) (realToFrac length_) colorPtr

foreign import ccall unsafe "raylib.h DrawGrid" c_DrawGrid :: CInt -> CFloat -> IO ()
drawGrid :: Int -> Float -> IO ()
drawGrid slices spacing = c_DrawGrid (fromIntegral slices) (realToFrac spacing)

foreign import ccall unsafe "models.h WrappedDrawBillboard" c_WrappedDrawBillboard :: Ptr Camera3D -> Ptr Texture2D -> Ptr Vector3 -> CFloat -> Ptr Color -> IO ()
drawBillboard :: Camera3D -> Texture2D -> Vector3 -> Float -> Color -> IO ()
drawBillboard camera texture center size tint =
  with camera $ \cameraPtr ->
    withTexture2D texture $ \texturePtr ->
      with center $ \centerPtr ->
        with tint $ \tintPtr ->
          c_WrappedDrawBillboard cameraPtr texturePtr centerPtr (realToFrac size) tintPtr

foreign import ccall unsafe "models.h LoadMaterials" c_LoadMaterials :: CString -> Ptr CInt -> IO (Ptr Material)
loadMaterials :: String -> Int -> IO Material
loadMaterials filename materialCount =
  withCString filename $ \cFilename ->
    with (fromIntegral materialCount) $ \materialCountPtr -> do
      materialPtr <- c_LoadMaterials cFilename materialCountPtr
      materialForeignPtr <- newForeignPtr c_WrappedUnloadMaterial materialPtr
      pure $ Material materialForeignPtr

foreign import ccall unsafe "models.h WrappedLoadMaterialDefault" c_WrappedLoadMaterialDefault :: IO (Ptr Material)
loadMaterialDefault :: IO Material
loadMaterialDefault = do
  materialPtr <- c_WrappedLoadMaterialDefault
  materialForeignPtr <- newForeignPtr c_WrappedUnloadMaterial materialPtr
  pure $ Material materialForeignPtr

foreign import ccall unsafe "models.h &WrappedUnloadMaterial" c_WrappedUnloadMaterial :: FunPtr (Ptr Material -> IO ())

foreign import ccall unsafe "models.h &WrappedUnloadMesh" c_WrappedUnloadMesh :: FunPtr (Ptr Mesh -> IO ())

foreign import ccall unsafe "models.h &WrappedUnloadModel" c_WrappedUnloadModel :: FunPtr (Ptr Model -> IO ())

foreign import ccall unsafe "models.h WrappedLoadModelFromMesh" c_WrappedLoadModelFromMesh :: Ptr Mesh -> IO (Ptr Model)
loadModelFromMesh :: Mesh -> IO Model
loadModelFromMesh mesh =
  withMesh mesh $ \meshPtr -> do
    modelPtr <- c_WrappedLoadModelFromMesh meshPtr
    modelForeignPtr <- newForeignPtr c_WrappedUnloadModel modelPtr
    pure $ Model modelForeignPtr

foreign import ccall unsafe "models.h WrappedGenMeshCube" c_WrappedGenMeshCube :: CFloat -> CFloat -> CFloat -> IO (Ptr Mesh)
genMeshCube :: Float -> Float -> Float -> IO Mesh
genMeshCube width height length_ = do
  meshPtr <- c_WrappedGenMeshCube (realToFrac width) (realToFrac height) (realToFrac length_)
  Mesh <$> newForeignPtr c_WrappedUnloadMesh meshPtr

foreign import ccall unsafe "models.h WrappedGenMeshCubicmap" c_WrappedGenMeshCubicmap :: Ptr Image -> Ptr Vector3 -> IO (Ptr Mesh)
genMeshCubicmap :: Image -> Vector3 -> IO Mesh
genMeshCubicmap cubicmap cubeSize =
  withImage cubicmap $ \cubicmapPtr ->
    with cubeSize $ \cubeSizePtr -> do
      meshPtr <- c_WrappedGenMeshCubicmap cubicmapPtr cubeSizePtr
      Mesh <$> newForeignPtr c_WrappedUnloadMesh meshPtr

foreign import ccall unsafe "models.h WrappedDrawModel" c_WrappedDrawModel :: Ptr Model -> Ptr Vector3 -> CFloat -> Ptr Color -> IO ()
drawModel :: Model -> Vector3 -> Float -> Color -> IO ()
drawModel model position scale tint =
  withModel model $ \modelPtr ->
    with position $ \positionPtr ->
      with tint $ \tintPtr ->
        c_WrappedDrawModel modelPtr positionPtr (realToFrac scale) tintPtr
