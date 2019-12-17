{-# LANGUAGE ForeignFunctionInterface #-}
module Core (

  -- * Window-related functions
  initWindow,
  closeWindow,
  isWindowReady,
  windowShouldClose,
  isWindowMinimized,
  toggleFullscreen,
  setWindowIcon,
  setWindowTitle,
  setWindowPosition,
  setWindowMonitor,
  setWindowMinSize,
  setWindowSize,
  getScreenWidth,
  getScreenHeight,

  -- * Cursor-related functions
  showCursor,
  hideCursor,
  isCursorHidden,
  enableCursor,
  disableCursor,

  -- * Drawing-related functions
  clearBackground,
  beginDrawing,
  endDrawing,
  -- TODO beginMode2D,
  -- TODO endMode2D,
  beginMode3D,
  endMode3D,
  -- TODO beginTextureMode,
  -- TODO endTextureMode,

  -- * Screen-space-related functions
  -- TODO getMouseRay,
  -- TODO getWorldToScreen,
  -- TODO getCameraMatrix,

  -- * Timing-related functions
  setTargetFPS,
  getFPS,
  getFrameTime,
  getTime,

  -- * Misc. functions
  showLogo,
  setConfigFlags,
  setTraceLogLevel,
  traceLog,
  takeScreenshot,

  -- * File management functions
  -- TODO getWorkingDirectory,
  -- TODO changeDirectory,
  -- TODO isFileDropped,
  -- TODO getDroppedFiles,
  -- TODO clearDroppedFiles,

  -- * Keyboard-related functions
  isKeyPressed,
  isKeyDown,
  isKeyReleased,
  isKeyUp,
  getKeyPressed,
  setExitKey,

  -- * Mouse-related functions
  isMouseButtonPressed,
  isMouseButtonDown,
  isMouseButtonReleased,
  isMouseButtonUp,
  getMouseX,
  getMouseY,
  getMousePosition,
  setMousePosition,
  getMouseWheelMove,

  -- * Gamepad-related functions
  -- TODO isGamepadAvailable,
  -- TODO getGamepadName,
  -- TODO isGamepadButtonPressed,
  -- TODO isGamepadButtonDown,
  -- TODO isGamepadButtonReleased,
  -- TODO isGamepadButtonUp,
  -- TODO getGamepadButtonPressed,
  -- TODO getGamepadAxisCount,
  -- TODO getGamepadAxisMovement,

  -- * Touch-related functions
  -- TODO getTouchX,
  -- TODO getTouchY,
  -- TODO getTouchPosition,

  -- * Gesture-related functions
  -- TODO setGestureEnabled,
  -- TODO isGestureDetected,
  -- TODO getGestureDetected,
  -- TODO getTouchPointsCount,
  -- TODO getGestureHoldDuration,
  -- TODO getGestureDragVector,
  -- TODO getGestureDragAngle,
  -- TODO getGesturePinchVector,
  -- TODO getGesturePinchAngle,

  -- * Camera-related functions
  setCameraMode,
  updateCamera,
  -- TODO setCameraPanControl,
  -- TODO setCameraAltControl,
  -- TODO setCameraSmoothZoomControl,
  -- TODO setCameraMoveControls,

) where
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Types
import Utils

#include "raylib.h"
#include "core.h"

foreign import ccall unsafe "raylib.h InitWindow" c_InitWindow :: CInt -> CInt -> CString -> IO ()
initWindow :: Int -- ^ width
           -> Int -- ^ height
           -> String -- ^ title
           -> IO ()
initWindow width height title =
  withCString title $ \cTitle ->
    c_InitWindow (fromIntegral width) (fromIntegral height) cTitle

foreign import ccall unsafe "raylib.h CloseWindow" c_CloseWindow :: IO ()
closeWindow :: IO ()
closeWindow = c_CloseWindow

foreign import ccall unsafe "core.h WrappedClearBackground" c_WrappedClearBackground :: Ptr Color -> IO ()
clearBackground :: Color -- ^ background color
                -> IO ()
clearBackground color =
  with color $ \colorPtr ->
    c_WrappedClearBackground colorPtr

foreign import ccall unsafe "raylib.h BeginDrawing" c_BeginDrawing :: IO ()
beginDrawing :: IO ()
beginDrawing = c_BeginDrawing

foreign import ccall unsafe "raylib.h EndDrawing" c_EndDrawing :: IO ()
endDrawing :: IO ()
endDrawing = c_EndDrawing

foreign import ccall unsafe "core.h WrappedBeginMode3D" c_WrappedBeginMode3D :: Ptr Camera3D -> IO ()
beginMode3D :: Camera3D -> IO ()
beginMode3D camera =
  with camera $ \cameraPtr ->
    c_WrappedBeginMode3D cameraPtr

foreign import ccall unsafe "raylib.h EndMode3D" c_EndMode3D :: IO ()
endMode3D :: IO ()
endMode3D = c_EndMode3D

foreign import ccall unsafe "raylib.h SetTraceLogLevel" c_SetTraceLogLevel :: CInt -> IO ()
setTraceLogLevel :: LogType -> IO ()
setTraceLogLevel logType = c_SetTraceLogLevel (fromIntegral (fromEnum logType))

foreign import ccall unsafe "raylib.h TraceLog" c_TraceLog :: CInt -> CString -> IO ()
traceLog :: LogType -> String -> IO ()
traceLog logType logMessage =
  withCString logMessage $ \cLogMessage ->
    c_TraceLog (fromIntegral (fromEnum logType)) cLogMessage

foreign import ccall unsafe "raylib.h IsWindowReady" c_IsWindowReady :: IO CBool
isWindowReady :: IO Bool
isWindowReady = toBool <$> c_IsWindowReady

foreign import ccall unsafe "raylib.h WindowShouldClose" c_WindowShouldClose :: IO CBool
windowShouldClose :: IO Bool
windowShouldClose = toBool <$> c_WindowShouldClose

foreign import ccall unsafe "raylib.h IsWindowMinimized" c_IsWindowMinimized :: IO CBool
isWindowMinimized :: IO Bool
isWindowMinimized = toBool <$> c_IsWindowMinimized

foreign import ccall unsafe "raylib.h ToggleFullscreen" c_ToggleFullscreen :: IO ()
toggleFullscreen :: IO ()
toggleFullscreen = c_ToggleFullscreen

foreign import ccall unsafe "core.h WrappedSetWindowIcon" c_WrappedSetWindowIcon :: Ptr Image -> IO ()
setWindowIcon :: Image -> IO ()
-- ^ This function keeps a copy of the given image in memory forever; this is a workaround for a bug in the underlying C libraries.
-- This function effectively leaks memory. Calling this function many times will cause more and more memory to be used.
-- Thus, ideally, this function should only be called once.
setWindowIcon image =
  withImage image $ \imagePtr -> do
    -- There is a bug in raylib. On GNOME 3 (and maybe others) if you call raylib's SetWindowIcon function
    -- too many times, or if the image you are using as the icon is freed to quickly after the call,
    -- you get a double free error. See: https://github.com/raysan5/raylib/issues/689
    c_WrappedSetWindowIcon imagePtr

foreign import ccall unsafe "raylib.h SetWindowTitle" c_SetWindowTitle :: CString -> IO ()
setWindowTitle :: String -> IO ()
setWindowTitle title =
  withCString title $ \cTitle ->
    c_SetWindowTitle cTitle

foreign import ccall unsafe "raylib.h SetWindowPosition" c_SetWindowPosition :: CInt -> CInt -> IO ()
setWindowPosition :: Int -> Int -> IO ()
setWindowPosition x y = c_SetWindowPosition (fromIntegral x) (fromIntegral y)

foreign import ccall unsafe "raylib.h SetWindowMonitor" c_SetWindowMonitor :: CInt -> IO ()
setWindowMonitor :: Int -> IO ()
setWindowMonitor monitor = c_SetWindowMonitor (fromIntegral monitor)

foreign import ccall unsafe "raylib.h SetWindowMinSize" c_SetWindowMinSize :: CInt -> CInt -> IO ()
setWindowMinSize :: Int -> Int -> IO ()
setWindowMinSize width height = c_SetWindowMinSize (fromIntegral width) (fromIntegral height)

foreign import ccall unsafe "raylib.h SetWindowSize" c_SetWindowSize :: CInt -> CInt -> IO ()
setWindowSize :: Int -> Int -> IO ()
setWindowSize width height = c_SetWindowSize (fromIntegral width) (fromIntegral height)

foreign import ccall unsafe "raylib.h GetScreenWidth" c_GetScreenWidth :: IO CInt
getScreenWidth :: IO Int
getScreenWidth = fromIntegral <$> c_GetScreenWidth

foreign import ccall unsafe "raylib.h GetScreenHeight" c_GetScreenHeight :: IO CInt
getScreenHeight :: IO Int
getScreenHeight = fromIntegral <$> c_GetScreenHeight

foreign import ccall unsafe "raylib.h ShowCursor" c_ShowCursor :: IO ()
showCursor :: IO ()
showCursor = c_ShowCursor

foreign import ccall unsafe "raylib.h HideCursor" c_HideCursor :: IO ()
hideCursor :: IO ()
hideCursor = c_HideCursor

foreign import ccall unsafe "raylib.h IsCursorHidden" c_IsCursorHidden :: IO CBool
isCursorHidden :: IO Bool
isCursorHidden = toBool <$> c_IsCursorHidden

foreign import ccall unsafe "raylib.h EnableCursor" c_EnableCursor :: IO ()
enableCursor :: IO ()
enableCursor = c_EnableCursor

foreign import ccall unsafe "raylib.h DisableCursor" c_DisableCursor :: IO ()
disableCursor :: IO ()
disableCursor = c_DisableCursor

foreign import ccall unsafe "raylib.h SetTargetFPS" c_SetTargetFPS :: CInt -> IO ()
setTargetFPS :: Int -> IO ()
setTargetFPS = c_SetTargetFPS . fromIntegral

foreign import ccall unsafe "raylib.h GetFPS" c_GetFPS :: IO CInt
getFPS :: IO Int
getFPS = fromIntegral <$> c_GetFPS

foreign import ccall unsafe "raylib.h GetFrameTime" c_GetFrameTime :: IO CFloat
getFrameTime :: IO Float
getFrameTime = realToFrac <$> c_GetFrameTime

foreign import ccall unsafe "raylib.h GetTime" c_GetTime :: IO CDouble
getTime :: IO Double
getTime = realToFrac <$> c_GetTime

foreign import ccall unsafe "raylib.h SetConfigFlags" c_SetConfigFlags :: CUChar -> IO ()
setConfigFlags :: [ConfigFlag] -> IO ()
setConfigFlags configFlags = c_SetConfigFlags (combineBitflags configFlags)

foreign import ccall unsafe "raylib.h ShowLogo" c_ShowLogo :: IO ()
showLogo :: IO ()
showLogo = c_ShowLogo

foreign import ccall unsafe "raylib.h TakeScreenshot" c_TakeScreenshot :: CString -> IO ()
takeScreenshot :: String -> IO ()
takeScreenshot fileName =
  withCString fileName $ \cFileName ->
    c_TakeScreenshot cFileName

foreign import ccall unsafe "raylib.h IsKeyPressed" c_IsKeyPressed :: CInt -> IO CBool
isKeyPressed :: KeyboardKey -> IO Bool
isKeyPressed key = toBool <$> c_IsKeyPressed (fromIntegral (fromEnum key))

foreign import ccall unsafe "raylib.h IsKeyDown" c_IsKeyDown :: CInt -> IO CBool
isKeyDown :: KeyboardKey -> IO Bool
isKeyDown key = toBool <$> c_IsKeyDown (fromIntegral (fromEnum key))

foreign import ccall unsafe "raylib.h IsKeyReleased" c_IsKeyReleased :: CInt -> IO CBool
isKeyReleased :: KeyboardKey -> IO Bool
isKeyReleased key = toBool <$> c_IsKeyReleased (fromIntegral (fromEnum key))

foreign import ccall unsafe "raylib.h IsKeyUp" c_IsKeyUp :: CInt -> IO CBool
isKeyUp :: KeyboardKey -> IO Bool
isKeyUp key = toBool <$> c_IsKeyUp (fromIntegral (fromEnum key))

foreign import ccall unsafe "raylib.h GetKeyPressed" c_GetKeyPressed :: IO CInt
getKeyPressed :: IO KeyboardKey
getKeyPressed = toEnum . fromIntegral <$> c_GetKeyPressed

foreign import ccall unsafe "raylib.h SetExitKey" c_SetExitKey :: CInt -> IO ()
setExitKey :: KeyboardKey -> IO ()
setExitKey key = c_SetExitKey (fromIntegral (fromEnum key))

foreign import ccall "raylib.h IsMouseButtonPressed" c_IsMouseButtonPressed :: CInt -> IO CBool
isMouseButtonPressed :: MouseButton -> IO Bool
isMouseButtonPressed mouseButton = toBool <$> c_IsMouseButtonPressed (fromIntegral (fromEnum mouseButton))

foreign import ccall "raylib.h IsMouseButtonDown" c_IsMouseButtonDown :: CInt -> IO CBool
isMouseButtonDown :: MouseButton -> IO Bool
isMouseButtonDown mouseButton = toBool <$> c_IsMouseButtonDown (fromIntegral (fromEnum mouseButton))

foreign import ccall "raylib.h IsMouseButtonReleased" c_IsMouseButtonReleased :: CInt -> IO CBool
isMouseButtonReleased :: MouseButton -> IO Bool
isMouseButtonReleased mouseButton = toBool <$> c_IsMouseButtonReleased (fromIntegral (fromEnum mouseButton))

foreign import ccall "raylib.h IsMouseButtonUp" c_IsMouseButtonUp :: CInt -> IO CBool
isMouseButtonUp :: MouseButton -> IO Bool
isMouseButtonUp mouseButton = toBool <$> c_IsMouseButtonUp (fromIntegral (fromEnum mouseButton))

foreign import ccall "raylib.h GetMouseX" c_GetMouseX :: IO CInt
getMouseX :: IO Int
getMouseX = fromIntegral <$> c_GetMouseX

foreign import ccall "raylib.h GetMouseY" c_GetMouseY :: IO CInt
getMouseY :: IO Int
getMouseY = fromIntegral <$> c_GetMouseY

foreign import ccall "core.h WrappedGetMousePosition" c_WrappedGetMousePosition :: Ptr Vector2 -> IO ()
getMousePosition :: IO Vector2
getMousePosition =
  alloca $ \vector2ResultPtr -> do
    c_WrappedGetMousePosition vector2ResultPtr
    peek vector2ResultPtr

foreign import ccall "core.h WrappedSetMousePosition" c_SetMousePosition :: CInt -> CInt -> IO ()
setMousePosition :: Int -> Int -> IO ()
setMousePosition x y = c_SetMousePosition (fromIntegral x) (fromIntegral y)

foreign import ccall "raylib.h GetMouseWheelMove" c_GetMouseWheelMove :: IO CInt
getMouseWheelMove :: IO Int
getMouseWheelMove = fromIntegral <$> c_GetMouseWheelMove

foreign import ccall "core.h WrappedSetCameraMode" c_WrappedSetCameraMode :: Ptr Camera3D -> CInt -> IO ()
setCameraMode :: Camera3D -> CameraMode -> IO ()
setCameraMode camera mode =
  with camera $ \cameraPtr ->
    c_WrappedSetCameraMode cameraPtr (fromIntegral (fromEnum mode))

foreign import ccall "raylib.h UpdateCamera" c_UpdateCamera :: Ptr Camera3D -> IO ()
updateCamera :: Camera3D -> IO Camera3D
updateCamera camera =
  with camera $ \cameraPtr -> do
    c_UpdateCamera cameraPtr
    peek cameraPtr
