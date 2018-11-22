import Colors
import Core
import Models
import Text
import Types

main :: IO ()
main = do
  let screenWidth = 800
      screenHeight = 450
  setConfigFlags [Vsync, Msaa4x]
  initWindow screenWidth screenHeight "raylib [core] example - 3d mode"
  setTargetFPS 60
  let camera = (Camera3D (Vector3 4 4 4) (Vector3 0 0 0) (Vector3 0 1 0) 45 Perspective)
  setCameraMode camera Orbital
  loop camera
  closeWindow
  where
    loop :: Camera3D -> IO ()
    loop camera = do
      camera' <- updateCamera camera
      beginDrawing
      clearBackground rayWhite
      beginMode3D camera'
      drawCube (Vector3 0 0 0) 2 2 2 red
      drawCubeWires (Vector3 0 0 0) 2 2 2 maroon
      drawGrid 10 1
      endMode3D
      drawText "Welcome to the third dimension!" 10 40 20 darkGrey
      drawFPS 10 10
      endDrawing
      shouldClose <- windowShouldClose
      if shouldClose then pure () else loop camera'
