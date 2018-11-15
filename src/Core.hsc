module Core (

  -- * Window-related functions
  initWindow,
  closeWindow,
  -- TODO isWindowReady,
  -- TODO windowShouldClose,
  -- TODO isWindowMinimized,
  -- TODO toggleFullscreen,
  -- TODO setWindowIcon,
  -- TODO setWindowTitle,
  -- TODO setWindowPosition,
  -- TODO setWindowMonitor,
  -- TODO setWindowMinSize,
  -- TODO setWindowSize,
  -- TODO getScreenWidth,
  -- TODO getScreenHeight,

  -- * Cursor-related functions
  -- TODO showCursor,
  -- TODO hideCursor,
  -- TODO isCursorHidden,
  -- TODO enableCursor,
  -- TODO disableCursor,

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
  -- TODO setTraceLog,
  -- TODO traceLog,
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

) where
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Utils
import Foreign.Ptr
import Types

#include "raylib.h"
#include "core.h"

foreign import ccall unsafe "raylib.h InitWindow" c_InitWindow :: CInt -> CInt -> CString -> IO ()
initWindow :: Int -- ^ width
           -> Int -- ^ height
           -> String -- ^ title
           -> IO ()
initWindow width height title = do
  cTitle <- newCString title
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
