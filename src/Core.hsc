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
  beginMode2D,
  endMode2D,
  beginMode3D,
  endMode3D,
  beginTextureMode,
  endTextureMode,

  -- * Screen-space-related functions
  getMouseRay,
  getWorldToScreen,
  getCameraMatrix,

  -- * Timing-related functions
  setTargetFPS,
  getFPS,
  getFrameTime,
  getTime,

  -- * Misc. functions
  showLogo,
  setConfigFlags,
  setTraceLog,
  traceLog,
  takeScreenshot,

  -- * File management functions
  getWorkingDirectory,
  changeDirectory,
  isFileDropped,
  getDroppedFiles,
  clearDroppedFiles,

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
  isGamepadAvailable,
  getGamepadName,
  isGamepadButtonPressed,
  isGamepadButtonDown,
  isGamepadButtonReleased,
  isGamepadButtonUp,
  getGamepadButtonPressed,
  getGamepadAxisCount,
  getGamepadAxisMovement,

  -- * Touch-related functions
  getTouchX,
  getTouchY,
  getTouchPosition,

  -- * Gesture-related functions
  setGestureEnabled,
  isGestureDetected,
  getGestureDetected,
  getTouchPointsCount,
  getGestureHoldDuration,
  getGestureDragVector,
  getGestureDragAngle,
  getGesturePinchVector,
  getGesturePinchAngle,

  -- * Camera-related functions
  setCameraMode,
  updateCamera,
  setCameraPanControl,
  setCameraAltControl,
  setCameraSmoothZoomControl,
  setCameraMoveControls,

) where
