module Core (
    initWindow
  , closeWindow
  , isWindowReady
  , windowShouldClose
  , isWindowMinimized
  , toggleFullscreen
  , setWindowIcon
  , setWindowTitle
  , setWindowPosition
  , setWindowMonitor
  , setWindowMinSize
  , setWindowSize
  , getScreenWidth
  , getScreenHeight
  , showCursor
  , hideCursor
  , isCursorHidden
  , enableCursor
  , disableCursor
  , clearBackground
  , beginDrawing
  , endDrawing
  , beginMode2D
  , endMode2D
  , beginMode3D
  , endMode3D
  , beginTextureMode
  , endTextureMode
  , getMouseRay
  , getWorldToScreen
  , getCameraMatrix
) where
import qualified Internal.Core
import           Internal.Structs (
    Color
  , Vector2
  , Vector3
  , Matrix
  , Image
  , RenderTexture2D
  , Camera3D
  , Camera2D
  , Ray
  )

-----------------------------------------
-- Window-related functions
-----------------------------------------

initWindow :: Int -> Int -> String -> IO ()
initWindow width height title = Internal.Core.initWindow width height title

closeWindow :: IO ()
closeWindow = Internal.Core.closeWindow

isWindowReady :: IO Bool
isWindowReady = Internal.Core.isWindowReady

windowShouldClose :: IO Bool
windowShouldClose = Internal.Core.windowShouldClose

isWindowMinimized :: IO Bool
isWindowMinimized = Internal.Core.isWindowMinimized

toggleFullscreen :: IO ()
toggleFullscreen = Internal.Core.toggleFullscreen

setWindowIcon :: Image -> IO ()
setWindowIcon image = Internal.Core.setWindowIcon image

setWindowTitle :: String -> IO ()
setWindowTitle title = Internal.Core.setWindowTitle title

setWindowPosition :: Int -> Int -> IO ()
setWindowPosition x y = Internal.Core.setWindowPosition x y

setWindowMonitor :: Int -> IO ()
setWindowMonitor monitor = Internal.Core.setWindowMonitor monitor

setWindowMinSize :: Int -> Int -> IO ()
setWindowMinSize width height = Internal.Core.setWindowMinSize width height

setWindowSize :: Int -> Int -> IO ()
setWindowSize width height = Internal.Core.setWindowSize width height

getScreenWidth :: IO Int
getScreenWidth = Internal.Core.getScreenWidth

getScreenHeight :: IO Int
getScreenHeight = Internal.Core.getScreenHeight

showCursor :: IO ()
showCursor = Internal.Core.showCursor

hideCursor :: IO ()
hideCursor = Internal.Core.hideCursor

isCursorHidden :: IO Bool
isCursorHidden = Internal.Core.isCursorHidden

enableCursor :: IO ()
enableCursor = Internal.Core.enableCursor

disableCursor :: IO ()
disableCursor = Internal.Core.disableCursor

clearBackground :: Color -> IO ()
clearBackground color = Internal.Core.clearBackground color

beginDrawing :: IO ()
beginDrawing = Internal.Core.beginDrawing

endDrawing :: IO ()
endDrawing = Internal.Core.endDrawing

beginMode2D :: Camera2D -> IO ()
beginMode2D camera = Internal.Core.beginMode2D camera

endMode2D :: IO ()
endMode2D = Internal.Core.endMode2D

beginMode3D :: Camera3D -> IO ()
beginMode3D camera = Internal.Core.beginMode3D camera

endMode3D :: IO ()
endMode3D = Internal.Core.endMode3D

beginTextureMode :: RenderTexture2D -> IO ()
beginTextureMode target = Internal.Core.beginTextureMode target

endTextureMode :: IO ()
endTextureMode = Internal.Core.endTextureMode

getMouseRay :: Vector2 -> Camera3D -> IO Ray
getMouseRay mousePosition camera = Internal.Core.getMouseRay mousePosition camera

getWorldToScreen :: Vector3 -> Camera3D -> IO Vector2
getWorldToScreen position camera = Internal.Core.getWorldToScreen position camera

getCameraMatrix :: Camera3D -> IO Matrix
getCameraMatrix camera = Internal.Core.getCameraMatrix camera
