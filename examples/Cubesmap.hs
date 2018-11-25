import Colors
import Core
import Models
import Shapes
import Text
import Textures
import Types

main :: IO ()
main = do
  let screenWidth = 800
      screenHeight = 450
  setConfigFlags [Vsync, Msaa4x]
  initWindow screenWidth screenHeight "raylib [models] example - cubesmap loading and drawing"
  let camera = (Camera3D (Vector3 16 14 16) (Vector3 0 0 0) (Vector3 0 1 0) 45 Perspective)
  image <- loadImage "test_data/cubicmap.png"
  cubicmap <- loadTextureFromImage image
  mesh <- genMeshCubicmap image (Vector3 1 1 1)
  model <- loadModelFromMesh mesh
  texture <- loadTexture "test_data/cubicmap_atlas.png"
  modelSetMaterialMap model Albedo (MaterialMap texture black 0)
  setCameraMode camera Orbital
  setTargetFPS 60
  loop camera cubicmap model
  closeWindow
  where
    loop camera cubicmap model = do
      nextCamera <- updateCamera camera
      let mapPosition = Vector3 (-16) 0 (-8)
      screenWidth <- getScreenWidth
      beginDrawing
      clearBackground rayWhite
      beginMode3D nextCamera
      drawModel model mapPosition 1 white
      endMode3D
      w <- texture2DWidth cubicmap
      h <- texture2DHeight cubicmap
      drawTextureEx cubicmap (Vector2 (fromIntegral $ screenWidth - w * 4 - 20) 20) 0 4 white
      drawRectangleLines (screenWidth - w * 4 - 20) 20 (w*4) (h*4) green
      drawText "cubicmap image used to" 658 90 10 gray
      drawText "generate map 3d model" 658 104 10 gray
      drawFPS 10 10
      endDrawing
      shouldClose <- windowShouldClose
      if shouldClose then pure () else loop camera cubicmap model

