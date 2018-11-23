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
  -- TODO loadModelFromMesh,
  -- TODO unloadModel,

  -- * Mesh loading functions
  -- TODO loadMesh,
  -- TODO unloadMesh,
  -- TODO exportMesh,

  -- * Mesh manipulation functions
  -- TODO meshBoundingBox,
  -- TODO meshTangents,
  -- TODO meshBinormals,

  -- * Mesh generation functions
  -- TODO genMeshPlane,
  -- TODO genMeshCube,
  -- TODO genMeshSphere,
  -- TODO genMeshHemiSphere,
  -- TODO genMeshCylinder,
  -- TODO genMeshTorus,
  -- TODO genMeshKnot,
  -- TODO genMeshHeightmap,
  -- TODO genMeshCubicmap,

  -- * Material loading functions
  loadMaterial,
  loadMaterialDefault,

  -- * Model drawing functions
  -- TODO drawModel,
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

foreign import ccall unsafe "models.h WrappedLoadMaterial" c_WrappedLoadMaterial :: CString -> IO (Ptr Material)
loadMaterial :: String -> IO Material
loadMaterial filename =
  withCString filename $ \cFilename -> do
    materialPtr <- c_WrappedLoadMaterial cFilename
    Material <$> newForeignPtr c_WrappedUnloadMaterial materialPtr

foreign import ccall unsafe "models.h WrappedLoadMaterialDefault" c_WrappedLoadMaterialDefault :: IO (Ptr Material)
loadMaterialDefault :: IO Material
loadMaterialDefault = do
  materialPtr <- c_WrappedLoadMaterialDefault
  Material <$> newForeignPtr c_WrappedUnloadMaterial materialPtr

foreign import ccall unsafe "models.h &WrappedUnloadMaterial" c_WrappedUnloadMaterial :: FunPtr (Ptr Material -> IO ())
