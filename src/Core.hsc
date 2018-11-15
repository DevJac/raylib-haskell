module Core (

  -- * Window-related functions
  -- TODO initWindow,
  -- TODO closeWindow,
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
  -- TODO clearBackground,
  -- TODO beginDrawing,
  -- TODO endDrawing,
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
