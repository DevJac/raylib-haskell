{-# LANGUAGE LambdaCase #-}
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
  , setTargetFPS
  , getFPS
  , getFrameTime
  , getTime
  , colorToInt
  , colorNormalize
  , colorToHSV
  , getColor
  , fade
  , showLogo
  , setConfigFlags
  , setTraceLog
  , traceLog
  , takeScreenshot
  , getRandomValue
  , isKeyPressed
  , isKeyDown
  , isKeyReleased
  , isKeyUp
  , getKeyPressed
  , setExitKey
  , isMouseButtonPressed
  , isMouseButtonDown
  , isMouseButtonReleased
  , isMouseButtonUp
  , getMouseX
  , getMouseY
  , getMousePosition
  , setMousePosition
  , getMouseWheelMove
  , setCameraMode
  , updateCamera
  , setCameraPanControl
  , setCameraAltControl
  , setCameraSmoothZoomControl
  , setCameraMoveControls
) where
import Control.Exception (ErrorCall, displayException, tryJust)
import Data.Bits (Bits, (.|.))
import Data.List (isInfixOf)
import qualified Internal.Core
import           Types (
    ConfigFlag
  , KeyboardKey
  , MouseButton
  , LogType
  , CameraType
  , Color
  , Vector2
  , Vector3
  , Vector4
  , Matrix
  , Image
  , RenderTexture2D
  , Camera3D
  , Camera2D
  , Ray
  , GetKeyPressedError(NoKeyPressed, UnknownKeyPressed)
  )

combineBitflags :: (Enum a, Num b, Data.Bits.Bits b) => [a] -> b
combineBitflags = foldr (\a b -> fromIntegral (fromEnum a) .|. b) 0

-----------------------------------------
-- Window related functions
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

-----------------------------------------
-- Cursor related functions
-----------------------------------------

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

-----------------------------------------
-- Drawing related functions
-----------------------------------------

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

-----------------------------------------
-- Screen-space related functions
-----------------------------------------

getMouseRay :: Vector2 -> Camera3D -> IO Ray
getMouseRay mousePosition camera = Internal.Core.getMouseRay mousePosition camera

getWorldToScreen :: Vector3 -> Camera3D -> IO Vector2
getWorldToScreen position camera = Internal.Core.getWorldToScreen position camera

getCameraMatrix :: Camera3D -> IO Matrix
getCameraMatrix camera = Internal.Core.getCameraMatrix camera

-----------------------------------------
-- Timing related functions
-----------------------------------------

setTargetFPS :: Int -> IO ()
setTargetFPS fps = Internal.Core.setTargetFPS fps

getFPS :: IO Int
getFPS = Internal.Core.getFPS

getFrameTime :: IO Float
getFrameTime = Internal.Core.getFrameTime

getTime :: IO Double
getTime = Internal.Core.getTime

-----------------------------------------
-- Color related functions
-----------------------------------------

colorToInt :: Color -> IO Int
colorToInt color = Internal.Core.colorToInt color

colorNormalize :: Color -> IO Vector4
colorNormalize color = Internal.Core.colorNormalize color

colorToHSV :: Color -> IO Vector3
colorToHSV color = Internal.Core.colorToHSV color

getColor :: Int -> IO Color
getColor hexValue = Internal.Core.getColor hexValue

fade :: Color -> Float -> IO Color
fade color alpha = Internal.Core.fade color alpha

-----------------------------------------
-- Misc. functions
-----------------------------------------

showLogo :: IO ()
showLogo = Internal.Core.showLogo

setConfigFlags :: [ConfigFlag] -> IO ()
setConfigFlags flags = Internal.Core.setConfigFlags (combineBitflags flags)

setTraceLog :: [LogType] -> IO ()
setTraceLog types = Internal.Core.setTraceLog (combineBitflags types)

traceLog :: LogType -> String -> IO ()
traceLog logType text = Internal.Core.traceLog (fromIntegral (fromEnum logType)) text

takeScreenshot :: String -> IO ()
takeScreenshot fileName = Internal.Core.takeScreenshot fileName

getRandomValue :: Int -> Int -> IO Int
getRandomValue min' max' = Internal.Core.getRandomValue min' max'

-----------------------------------------
-- File management functions (TODO)
-----------------------------------------

-----------------------------------------
-- Persistend storage management (TODO)
-----------------------------------------

-----------------------------------------
-- Input related functions: keyboard
-----------------------------------------

isKeyPressed :: KeyboardKey -> IO Bool
isKeyPressed key = Internal.Core.isKeyPressed (fromEnum key)

isKeyDown :: KeyboardKey -> IO Bool
isKeyDown key = Internal.Core.isKeyPressed (fromEnum key)

isKeyReleased :: KeyboardKey -> IO Bool
isKeyReleased key = Internal.Core.isKeyReleased (fromEnum key)

isKeyUp :: KeyboardKey -> IO Bool
isKeyUp key = Internal.Core.isKeyUp (fromEnum key)

getKeyPressed :: IO (Either GetKeyPressedError KeyboardKey)
getKeyPressed = do
    Internal.Core.getKeyPressed >>= \case
        -1 -> pure $ Left NoKeyPressed
        i  -> tryJust toEnumCannotMatch (pure (toEnum i :: KeyboardKey))
  where
    toEnumCannotMatch :: ErrorCall -> Maybe GetKeyPressedError
    toEnumCannotMatch e = if "KeyboardKey.toEnum: Cannot match " `isInfixOf` (displayException e)
                          then Just UnknownKeyPressed
                          else Nothing

setExitKey :: KeyboardKey -> IO ()
setExitKey key = Internal.Core.setExitKey (fromEnum key)

-----------------------------------------
-- Input related functions: gamepads (TODO)
-----------------------------------------

-----------------------------------------
-- Input related functions: mouse
-----------------------------------------

isMouseButtonPressed :: MouseButton -> IO Bool
isMouseButtonPressed button = Internal.Core.isMouseButtonPressed (fromEnum button)

isMouseButtonDown :: MouseButton -> IO Bool
isMouseButtonDown button = Internal.Core.isMouseButtonDown (fromEnum button)

isMouseButtonReleased :: MouseButton -> IO Bool
isMouseButtonReleased button = Internal.Core.isMouseButtonReleased (fromEnum button)

isMouseButtonUp :: MouseButton -> IO Bool
isMouseButtonUp button = Internal.Core.isMouseButtonUp (fromEnum button)

getMouseX :: IO Int
getMouseX = Internal.Core.getMouseX

getMouseY :: IO Int
getMouseY = Internal.Core.getMouseY

getMousePosition :: IO Vector2
getMousePosition = Internal.Core.getMousePosition

setMousePosition :: Vector2 -> IO ()
setMousePosition position = Internal.Core.setMousePosition position

getMouseWheelMove :: IO Int
getMouseWheelMove = Internal.Core.getMouseWheelMove

-----------------------------------------
-- Input related functions: touch (TODO)
-----------------------------------------

-----------------------------------------
-- Gesture related functions (TODO)
-----------------------------------------

-----------------------------------------
-- Camera related functions
-----------------------------------------

setCameraMode :: Camera3D -> CameraType -> IO ()
setCameraMode camera mode = Internal.Core.setCameraMode camera (fromEnum mode)

updateCamera :: Camera3D -> IO Camera3D
updateCamera camera = Internal.Core.updateCamera camera

setCameraPanControl :: KeyboardKey -> IO ()
setCameraPanControl panKey = Internal.Core.setCameraPanControl (fromEnum panKey)

setCameraAltControl :: KeyboardKey -> IO ()
setCameraAltControl altKey = Internal.Core.setCameraAltControl (fromEnum altKey)

setCameraSmoothZoomControl :: KeyboardKey -> IO ()
setCameraSmoothZoomControl szKey = Internal.Core.setCameraSmoothZoomControl (fromEnum szKey)

setCameraMoveControls :: KeyboardKey -> KeyboardKey -> KeyboardKey -> KeyboardKey -> KeyboardKey -> KeyboardKey -> IO ()
setCameraMoveControls frontKey backKey rightKey leftKey upKey downKey =
    Internal.Core.setCameraMoveControls
        (fromEnum frontKey)
        (fromEnum backKey)
        (fromEnum rightKey)
        (fromEnum leftKey)
        (fromEnum upKey)
        (fromEnum downKey)
