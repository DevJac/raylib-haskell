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
  -- TODO beginMode3D,
  -- TODO endMode3D,
  -- TODO beginTextureMode,
  -- TODO endTextureMode,

  -- * Screen-space-related functions
  -- TODO getMouseRay,
  -- TODO getWorldToScreen,
  -- TODO getCameraMatrix,

  -- * Timing-related functions
  -- TODO setTargetFPS,
  -- TODO getFPS,
  -- TODO getFrameTime,
  -- TODO getTime,

  -- * Misc. functions
  -- TODO showLogo,
  -- TODO setConfigFlags,
  setTraceLog,
  traceLog,
  -- TODO takeScreenshot,

  -- * File management functions
  -- TODO getWorkingDirectory,
  -- TODO changeDirectory,
  -- TODO isFileDropped,
  -- TODO getDroppedFiles,
  -- TODO clearDroppedFiles,

  -- * Keyboard-related functions
  -- TODO isKeyPressed,
  -- TODO isKeyDown,
  -- TODO isKeyReleased,
  -- TODO isKeyUp,
  -- TODO getKeyPressed,
  -- TODO setExitKey,

  -- * Mouse-related functions
  -- TODO isMouseButtonPressed,
  -- TODO isMouseButtonDown,
  -- TODO isMouseButtonReleased,
  -- TODO isMouseButtonUp,
  -- TODO getMouseX,
  -- TODO getMouseY,
  -- TODO getMousePosition,
  -- TODO setMousePosition,
  -- TODO getMouseWheelMove,

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
  -- TODO setCameraMode,
  -- TODO updateCamera,
  -- TODO setCameraPanControl,
  -- TODO setCameraAltControl,
  -- TODO setCameraSmoothZoomControl,
  -- TODO setCameraMoveControls,

  -- TODO Remove the following function
  cTest,

) where
import Control.Concurrent
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Utils
import Foreign.Ptr
import Types
import Utils

#include "raylib.h"
#include "core.h"

foreign import ccall unsafe "core.h CTest" c_CTest :: IO ()
cTest :: IO ()
cTest = c_CTest

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

foreign import ccall unsafe "raylib.h SetTraceLog" c_SetTraceLog :: CUChar -> IO ()
setTraceLog :: [LogType] -> IO ()
setTraceLog logTypes = c_SetTraceLog (combineBitflags logTypes)

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
setWindowIcon (Image imageForeignPtr) =
  withForeignPtr imageForeignPtr $ \imagePtr -> do
    c_WrappedSetWindowIcon imagePtr
    -- There is a bug in raylib. On GNOME 3 (and maybe others) if you call raylib's SetWindowIcon function
    -- too many times, or if the image you are using as the icon is freed to quickly after the call,
    -- you get a double free error. See: https://github.com/raysan5/raylib/issues/689
    -- We use forkIO, threadDelay, and touchForeignPtr below to work around this issue.
    _ <- forkIO $ do threadDelay (10 * 1000 * 1000)
                     touchForeignPtr imageForeignPtr
    pure ()

foreign import ccall unsafe "raylib.h SetWindowTitle" c_SetWindowTitle :: CString -> IO ()
setWindowTitle :: String -> IO ()
setWindowTitle title =
  withCString title $ \titlePtr ->
    c_SetWindowTitle titlePtr

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
