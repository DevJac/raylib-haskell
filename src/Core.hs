{-|
This module contains bindings to most of the functions in raylib Core. See: https://www.raylib.com/cheatsheet/cheatsheet.html

All functions from raylib Core have bindings except for the following:

  * The 5 color-related functions

  * @getRandomValue@

  * Some of the file management functions

  * Persistent storage management functions

These functions are convenient to have in C, but not necessary in Haskell.
-}
{-# LANGUAGE LambdaCase #-}
module Core (
    -- * Window-related functions
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
  -- * Cursor-related functions
  , showCursor
  , hideCursor
  , isCursorHidden
  , enableCursor
  , disableCursor
  -- * Drawing-related functions
  , clearBackground
  , beginDrawing
  , endDrawing
  , beginMode2D
  , endMode2D
  , beginMode3D
  , endMode3D
  , beginTextureMode
  , endTextureMode
  -- * Screen-space-related functions
  , getMouseRay
  , getWorldToScreen
  , getCameraMatrix
  -- * Timing-related functions
  , setTargetFPS
  , getFPS
  , getFrameTime
  , getTime
  -- * Misc. functions
  , showLogo
  , setConfigFlags
  , setTraceLog
  , traceLog
  , takeScreenshot
  -- * File management functions
  , getWorkingDirectory
  , changeDirectory
  , isFileDropped
  , getDroppedFiles
  , clearDroppedFiles
  -- * Keyboard-related functions
  , isKeyPressed
  , isKeyDown
  , isKeyReleased
  , isKeyUp
  , getKeyPressed
  , setExitKey
  -- * Gamepad-related functions
  , isGamepadAvailable
  , isGamepadName
  , getGamepadName
  , isGamepadButtonPressed
  , isGamepadButtonDown
  , isGamepadButtonReleased
  , isGamepadButtonUp
  , getGamepadButtonPressed
  , getGamepadAxisCount
  , getGamepadAxisMovement
  -- * Mouse-related functions
  , isMouseButtonPressed
  , isMouseButtonDown
  , isMouseButtonReleased
  , isMouseButtonUp
  , getMouseX
  , getMouseY
  , getMousePosition
  , setMousePosition
  , getMouseWheelMove
  -- * Touch-related functions
  , getTouchX
  , getTouchY
  , getTouchPosition
  -- * Gesture-related functions
  , setGesturesEnabled
  , isGestureDetected
  , getGestureDetected
  , getTouchPointsCount
  , getGestureHoldDuration
  , getGestureDragVector
  , getGestureDragAngle
  , getGesturePinchVector
  , getGesturePinchAngle
  -- * Camera-related functions
  , setCameraMode
  , updateCamera
  , setCameraPanControl
  , setCameraAltControl
  , setCameraSmoothZoomControl
  , setCameraMoveControls
  ) where
import Control.Exception (ErrorCall, displayException, tryJust)
import Data.List (isInfixOf)
import Foreign.C.String (peekCString)
import Foreign.Marshal.Array (peekArray)
import Foreign.Marshal.Utils (with)
import Foreign.Storable (peek)
import qualified Internal.Bindings as Bindings
import Internal.Utils (combineBitflags)
import Types (
    Color
  , Vector2
  , Vector3
  , Matrix
  , Camera3D
  , Camera2D
  , Ray
  , Image
  , RenderTexture2D
  , CameraType
  , ConfigFlag
  , LogType
  , Gamepad
  , GamepadButton
  , GamepadAxis
  , Gesture
  , KeyboardKey
  , MouseButton
  , GetPressedError (NothingPressed, UnknownPressed)
  )

initWindow :: Int -- ^ width
           -> Int -- ^ height
           -> String -- ^ title
           -> IO ()
initWindow = Bindings.initWindow

closeWindow :: IO ()
closeWindow = Bindings.closeWindow

isWindowReady :: IO Bool
isWindowReady = Bindings.isWindowReady

-- | Check if the exit key (escape by default) or window manager has requested the window to close
--
-- See: 'setExitKey'
windowShouldClose :: IO Bool
windowShouldClose = Bindings.windowShouldClose

isWindowMinimized :: IO Bool
isWindowMinimized = Bindings.isWindowMinimized

toggleFullscreen :: IO ()
toggleFullscreen = Bindings.toggleFullscreen

setWindowIcon :: Image -- ^ icon image
              -> IO ()
setWindowIcon = Bindings.setWindowIcon

setWindowTitle :: String -- ^ title
               -> IO ()
setWindowTitle = Bindings.setWindowTitle

setWindowPosition :: Int -- ^ x position
                  -> Int -- ^ y position
                  -> IO ()
setWindowPosition = Bindings.setWindowPosition

-- | Set monitor to use for fullscreen mode
setWindowMonitor :: Int -- ^ monitor
                 -> IO ()
setWindowMonitor = Bindings.setWindowMonitor

setWindowMinSize :: Int -- ^ width
                 -> Int -- ^ height
                 -> IO ()
setWindowMinSize = Bindings.setWindowMinSize

setWindowSize :: Int -- ^ width
              -> Int -- ^ height
              -> IO ()
setWindowSize = Bindings.setWindowSize

getScreenWidth :: IO Int
getScreenWidth = Bindings.getScreenWidth

getScreenHeight :: IO Int
getScreenHeight = Bindings.getScreenHeight

showCursor :: IO ()
showCursor = Bindings.showCursor

hideCursor :: IO ()
hideCursor = Bindings.hideCursor

isCursorHidden :: IO Bool
isCursorHidden = Bindings.isCursorHidden

enableCursor :: IO ()
enableCursor = Bindings.enableCursor

disableCursor :: IO ()
disableCursor = Bindings.disableCursor

clearBackground :: Color -- ^ background color
                -> IO ()
clearBackground = Bindings.clearBackground

-- | Setup canvas (framebuffer) for drawing
--
-- If drawing functions seem to not be working, it might be because you haven't called 'beginDrawing' first.
beginDrawing :: IO ()
beginDrawing = Bindings.beginDrawing

-- | End canvas drawing and swap buffer (double buffering)
endDrawing :: IO ()
endDrawing = Bindings.endDrawing

beginMode2D :: Camera2D -- ^ initial 2D mode camera
            -> IO ()
beginMode2D = Bindings.beginMode2D

endMode2D :: IO ()
endMode2D = Bindings.endMode2D

beginMode3D :: Camera3D -- ^ initial 3D mode camera
            -> IO ()
beginMode3D = Bindings.beginMode3D

endMode3D :: IO ()
endMode3D = Bindings.endMode3D

beginTextureMode :: RenderTexture2D -- ^ render texture for drawing
                 -> IO ()
beginTextureMode = Bindings.beginTextureMode

endTextureMode :: IO ()
endTextureMode = Bindings.endTextureMode

getMouseRay :: Vector2 -- ^ mouse position
            -> Camera3D
            -> IO Ray
getMouseRay = Bindings.getMouseRay

getWorldToScreen :: Vector3 -- ^ position
                 -> Camera3D
                 -> IO Vector2
getWorldToScreen = Bindings.getWorldToScreen

getCameraMatrix :: Camera3D
                -> IO Matrix -- ^ camera transform matrix (view matrix)
getCameraMatrix = Bindings.getCameraMatrix

setTargetFPS :: Int -- ^ maximum FPS (frames per second)
             -> IO ()
setTargetFPS = Bindings.setTargetFPS

getFPS :: IO Int -- ^ frame per second
getFPS = Bindings.getFPS

getFrameTime :: IO Float -- ^ time (in seconds) for last frame drawn
getFrameTime = Bindings.getFrameTime

getTime :: IO Double -- ^ elapsed time (in seconds) since 'initWindow'
getTime = Bindings.getTime

-- | Show raylib logo at start; can also be done with a flag using 'setConfigFlags'
showLogo :: IO ()
showLogo = Bindings.showLogo

setConfigFlags :: [ConfigFlag] -> IO ()
setConfigFlags = Bindings.setConfigFlags . combineBitflags

-- | Set allowed trace log message types
--
-- This can be used to make raylib's internal logging more or less verbose.
setTraceLog :: [LogType] -> IO ()
setTraceLog = Bindings.setTraceLog . combineBitflags

-- | Emit a log message using raylib's logging system
traceLog :: LogType -- ^ log level
         -> String -- ^ log message
         -> IO ()
traceLog logType logMessage = Bindings.traceLog (fromIntegral (fromEnum logType)) logMessage

takeScreenshot :: String -- ^ filename
               -> IO ()
takeScreenshot = Bindings.takeScreenshot

getWorkingDirectory :: IO String -- ^ raylib's current working directory
getWorkingDirectory = Bindings.getWorkingDirectory

changeDirectory :: String -- ^ raylib's new working directory
                -> IO Bool -- ^ success flag
changeDirectory = Bindings.changeDirectory

-- | Has a file been dragged and dropped into the raylib window?
isFileDropped :: IO Bool
isFileDropped = Bindings.isFileDropped

-- | Which files have been dragged and dropped into the raylib window?
--
-- See: 'clearDroppedFiles'
getDroppedFiles :: IO [String] -- ^ dropped filenames
getDroppedFiles = do
  (droppedFiles, count) <- Bindings.getDroppedFiles
  traverse peekCString =<< peekArray (fromIntegral count) droppedFiles

-- | Clear the list of files that have been dragged and dropped into the raylib window
--
-- See: 'getDroppedFiles'
clearDroppedFiles :: IO ()
clearDroppedFiles = Bindings.clearDroppedFiles

-- | Has the given key been pressed?
isKeyPressed :: KeyboardKey -> IO Bool
isKeyPressed = Bindings.isKeyPressed . fromEnum

-- | Is the given key being pressed?
isKeyDown :: KeyboardKey -> IO Bool
isKeyDown = Bindings.isKeyPressed . fromEnum

-- | Has the given key been released?
isKeyReleased :: KeyboardKey -> IO Bool
isKeyReleased = Bindings.isKeyReleased . fromEnum

-- | Is the given key NOT being pressed?
isKeyUp :: KeyboardKey -> IO Bool
isKeyUp = Bindings.isKeyUp . fromEnum

-- | Get last key pressed
--
-- It's possible that no key has ever been pressed, or that the last key pressed is unknown to these raylib bindings. This is the reason for the @Either GetPressedError KeyboardKey@ type.
getKeyPressed :: IO (Either GetPressedError KeyboardKey)
getKeyPressed = do
  Bindings.getKeyPressed >>= \case
    -1 -> pure $ Left NothingPressed
    i  -> tryJust toEnumCannotMatch (pure (toEnum i :: KeyboardKey))
  where
    toEnumCannotMatch :: ErrorCall -> Maybe GetPressedError
    toEnumCannotMatch e = if "KeyboardKey.toEnum: Cannot match " `isInfixOf` (displayException e)
                          then Just UnknownPressed
                          else Nothing

-- | Set a custom key to exit the program. The default is escape.
--
-- See: 'windowShouldClose'
setExitKey :: KeyboardKey -> IO ()
setExitKey = Bindings.setExitKey . fromEnum

isGamepadAvailable :: Gamepad
                   -> IO Bool
isGamepadAvailable = Bindings.isGamepadAvailable . fromEnum

-- | Does the given 'Gamepad' have the given @name@?
isGamepadName :: Gamepad
              -> String -- ^ name
              -> IO Bool
isGamepadName gamepad name = Bindings.isGamepadName (fromEnum gamepad) name

getGamepadName :: Gamepad
               -> IO String -- ^ gamepad name
getGamepadName = Bindings.getGamepadName . fromEnum

isGamepadButtonPressed :: Gamepad -> GamepadButton -> IO Bool
isGamepadButtonPressed gamepad button = Bindings.isGamepadButtonPressed (fromEnum gamepad) (fromEnum button)

isGamepadButtonDown :: Gamepad -> GamepadButton -> IO Bool
isGamepadButtonDown gamepad button = Bindings.isGamepadButtonDown (fromEnum gamepad) (fromEnum button)

isGamepadButtonReleased :: Gamepad -> GamepadButton -> IO Bool
isGamepadButtonReleased gamepad button = Bindings.isGamepadButtonReleased (fromEnum gamepad) (fromEnum button)

isGamepadButtonUp :: Gamepad -> GamepadButton -> IO Bool
isGamepadButtonUp gamepad button = Bindings.isGamepadButtonUp (fromEnum gamepad) (fromEnum button)

getGamepadButtonPressed :: IO (Either GetPressedError GamepadButton)
getGamepadButtonPressed = do
  Bindings.getGamepadButtonPressed >>= \case
    -1 -> pure $ Left NothingPressed
    i  -> tryJust toEnumCannotMatch (pure (toEnum i :: GamepadButton))
  where
    toEnumCannotMatch :: ErrorCall -> Maybe GetPressedError
    toEnumCannotMatch e = if "GamepadButton.toEnum: Cannot match " `isInfixOf` (displayException e)
                          then Just UnknownPressed
                          else Nothing

getGamepadAxisCount :: Gamepad -> IO Int
getGamepadAxisCount = Bindings.getGamepadAxisCount . fromEnum

getGamepadAxisMovement :: Gamepad -> GamepadAxis -> IO Float
getGamepadAxisMovement gamepad axis = Bindings.getGamepadAxisMovement (fromEnum gamepad) (fromEnum axis)

isMouseButtonPressed :: MouseButton -> IO Bool
isMouseButtonPressed = Bindings.isMouseButtonPressed . fromEnum

isMouseButtonDown :: MouseButton -> IO Bool
isMouseButtonDown = Bindings.isMouseButtonDown . fromEnum

isMouseButtonReleased :: MouseButton -> IO Bool
isMouseButtonReleased = Bindings.isMouseButtonReleased . fromEnum

isMouseButtonUp :: MouseButton -> IO Bool
isMouseButtonUp = Bindings.isMouseButtonUp . fromEnum

getMouseX :: IO Int
getMouseX = Bindings.getMouseX

getMouseY :: IO Int
getMouseY = Bindings.getMouseY

getMousePosition :: IO Vector2
getMousePosition = Bindings.getMousePosition

setMousePosition :: Vector2 -> IO ()
setMousePosition = Bindings.setMousePosition

getMouseWheelMove :: IO Int
getMouseWheelMove = Bindings.getMouseWheelMove

getTouchX :: IO Int
getTouchX = Bindings.getTouchX

getTouchY :: IO Int
getTouchY = Bindings.getTouchY

getTouchPosition :: Int -> IO Vector2
getTouchPosition = Bindings.getTouchPosition

setGesturesEnabled :: [Gesture] -> IO ()
setGesturesEnabled = Bindings.setGesturesEnabled . combineBitflags

isGestureDetected :: Gesture -> IO Bool
isGestureDetected = Bindings.isGestureDetected . fromEnum

getGestureDetected :: IO (Either GetPressedError Gesture)
getGestureDetected = do
  Bindings.getGestureDetected >>= \case
    -1 -> pure $ Left NothingPressed
    i  -> tryJust toEnumCannotMatch (pure (toEnum i :: Gesture))
  where
    toEnumCannotMatch :: ErrorCall -> Maybe GetPressedError
    toEnumCannotMatch e = if "Gesture.toEnum: Cannot match " `isInfixOf` (displayException e)
                          then Just UnknownPressed
                          else Nothing

getTouchPointsCount :: IO Int
getTouchPointsCount = Bindings.getTouchPointsCount

getGestureHoldDuration :: IO Float -- ^ gesture hold time in milliseconds
getGestureHoldDuration = Bindings.getGestureHoldDuration

getGestureDragVector :: IO Vector2
getGestureDragVector = Bindings.getGestureDragVector

getGestureDragAngle :: IO Float
getGestureDragAngle = Bindings.getGestureDragAngle

getGesturePinchVector :: IO Vector2
getGesturePinchVector = Bindings.getGesturePinchVector

getGesturePinchAngle :: IO Float
getGesturePinchAngle = Bindings.getGesturePinchAngle

setCameraMode :: Camera3D -> CameraType -> IO ()
setCameraMode camera mode = Bindings.setCameraMode camera (fromEnum mode)

updateCamera :: Camera3D -> IO Camera3D
updateCamera camera = do
  with camera $ \p -> do
    Bindings.updateCamera p
    peek p

setCameraPanControl :: KeyboardKey -> IO ()
setCameraPanControl = Bindings.setCameraPanControl . fromEnum

setCameraAltControl :: KeyboardKey -> IO ()
setCameraAltControl = Bindings.setCameraAltControl . fromEnum

setCameraSmoothZoomControl :: KeyboardKey -> IO ()
setCameraSmoothZoomControl = Bindings.setCameraSmoothZoomControl . fromEnum

setCameraMoveControls :: KeyboardKey -> KeyboardKey -> KeyboardKey -> KeyboardKey -> KeyboardKey -> KeyboardKey -> IO ()
setCameraMoveControls frontKey backKey rightKey leftKey upKey downKey =
  Bindings.setCameraMoveControls
    (fromEnum frontKey)
    (fromEnum backKey)
    (fromEnum rightKey)
    (fromEnum leftKey)
    (fromEnum upKey)
    (fromEnum downKey)
