{-|
All functions from raylib Core except for the following:

  * The 5 color-related functions

  * @getRandomValue@

  * Some of the file management functions

  * Persistent storage management functions
-}
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
  , getMousePosition
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
import qualified Internal.Bindings as Bindings

initWindow :: Int -- ^ width
           -> Int -- ^ height
           -> String -- ^ title
           -> IO ()
initWindow = Bindings.initWindow

closeWindow :: IO ()
closeWindow = Bindings.closeWindow

isWindowReady :: IO Bool
isWindowReady = Bindings.isWindowReady

-- | Check if escape key or close button has been pressed
--
-- See: 'setExitKey'
windowShouldClose :: IO Bool
windowShouldClose = Bindings.windowShouldClose

-- | Check if window has been minimized or lost focus
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




-- TODO    // Cursor-related functions
-- TODO    void ShowCursor(void);                                                  // Shows cursor
-- TODO    void HideCursor(void);                                                  // Hides cursor
-- TODO    bool IsCursorHidden(void);                                              // Check if cursor is not visible
-- TODO    void EnableCursor(void);                                                // Enables cursor (unlock cursor)
-- TODO    void DisableCursor(void);                                               // Disables cursor (lock cursor)
-- TODO
-- TODO    // Drawing-related functions
-- TODO    void ClearBackground(Color color);                                      // Set background color (framebuffer clear color)
-- TODO    void BeginDrawing(void);                                                // Setup canvas (framebuffer) to start drawing
-- TODO    void EndDrawing(void);                                                  // End canvas drawing and swap buffers (double buffering)
-- TODO    void BeginMode2D(Camera2D camera);                                      // Initialize 2D mode with custom camera (2D)
-- TODO    void EndMode2D(void);                                                   // Ends 2D mode with custom camera
-- TODO    void BeginMode3D(Camera3D camera);                                      // Initializes 3D mode with custom camera (3D)
-- TODO    void EndMode3D(void);                                                   // Ends 3D mode and returns to default 2D orthographic mode
-- TODO    void BeginTextureMode(RenderTexture2D target);                          // Initializes render texture for drawing
-- TODO    void EndTextureMode(void);                                              // Ends drawing to render texture
-- TODO
-- TODO    // Screen-space-related functions
-- TODO    Ray GetMouseRay(Vector2 mousePosition, Camera camera);                  // Returns a ray trace from mouse position
-- TODO    Vector2 GetWorldToScreen(Vector3 position, Camera camera);              // Returns the screen space position for a 3d world space position
-- TODO    Matrix GetCameraMatrix(Camera camera);                                  // Returns camera transform matrix (view matrix)
-- TODO
-- TODO    // Timing-related functions
-- TODO    void SetTargetFPS(int fps);                                             // Set target FPS (maximum)
-- TODO    int GetFPS(void);                                                       // Returns current FPS
-- TODO    float GetFrameTime(void);                                               // Returns time in seconds for last frame drawn
-- TODO    double GetTime(void);                                                   // Returns elapsed time in seconds since InitWindow()
-- TODO
-- TODO    // Color-related functions
-- TODO    int ColorToInt(Color color);                                            // Returns hexadecimal value for a Color
-- TODO    Vector4 ColorNormalize(Color color);                                    // Returns color normalized as float [0..1]
-- TODO    Vector3 ColorToHSV(Color color);                                        // Returns HSV values for a Color
-- TODO    Color GetColor(int hexValue);                                           // Returns a Color struct from hexadecimal value
-- TODO    Color Fade(Color color, float alpha);                                   // Color fade-in or fade-out, alpha goes from 0.0f to 1.0f
-- TODO
-- TODO    // Misc. functions
-- TODO    void ShowLogo(void);                                                    // Activate raylib logo at startup (can be done with flags)
-- TODO    void SetConfigFlags(unsigned char flags);                               // Setup window configuration flags (view FLAGS)
-- TODO    void SetTraceLog(unsigned char types);                                  // Enable trace log message types (bit flags based)
-- TODO    void TraceLog(int logType, const char *text, ...);                      // Show trace log messages (LOG_INFO, LOG_WARNING, LOG_ERROR, LOG_DEBUG)
-- TODO    void TakeScreenshot(const char *fileName);                              // Takes a screenshot of current screen (saved a .png)
-- TODO    int GetRandomValue(int min, int max);                                   // Returns a random value between min and max (both included)
-- TODO
-- TODO    // Files management functions
-- TODO    bool IsFileExtension(const char *fileName, const char *ext);            // Check file extension
-- TODO    const char *GetExtension(const char *fileName);                         // Get pointer to extension for a filename string
-- TODO    const char *GetFileName(const char *filePath);                          // Get pointer to filename for a path string
-- TODO    const char *GetDirectoryPath(const char *fileName);                     // Get full path for a given fileName (uses static string)
-- TODO    const char *GetWorkingDirectory(void);                                  // Get current working directory (uses static string)
-- TODO    bool ChangeDirectory(const char *dir);                                  // Change working directory, returns true if success
-- TODO    bool IsFileDropped(void);                                               // Check if a file has been dropped into window
-- TODO    char **GetDroppedFiles(int *count);                                     // Get dropped files names
-- TODO    void ClearDroppedFiles(void);                                           // Clear dropped files paths buffer
-- TODO
-- TODO    // Persistent storage management
-- TODO    void StorageSaveValue(int position, int value);                         // Save integer value to storage file (to defined position)
-- TODO    int StorageLoadValue(int position);                                     // Load integer value from storage file (from defined position)
-- TODO    
-- TODO    // Input-related functions: keyboard
-- TODO    bool IsKeyPressed(int key);                                             // Detect if a key has been pressed once
-- TODO    bool IsKeyDown(int key);                                                // Detect if a key is being pressed
-- TODO    bool IsKeyReleased(int key);                                            // Detect if a key has been released once
-- TODO    bool IsKeyUp(int key);                                                  // Detect if a key is NOT being pressed
-- TODO    int GetKeyPressed(void);                                                // Get latest key pressed
-- TODO    void SetExitKey(int key);                                               // Set a custom key to exit program (default is ESC)
-- TODO
-- TODO    // Input-related functions: gamepads
-- TODO    bool IsGamepadAvailable(int gamepad);                                   // Detect if a gamepad is available
-- TODO    bool IsGamepadName(int gamepad, const char *name);                      // Check gamepad name (if available)
-- TODO    const char *GetGamepadName(int gamepad);                                // Return gamepad internal name id
-- TODO    bool IsGamepadButtonPressed(int gamepad, int button);                   // Detect if a gamepad button has been pressed once
-- TODO    bool IsGamepadButtonDown(int gamepad, int button);                      // Detect if a gamepad button is being pressed
-- TODO    bool IsGamepadButtonReleased(int gamepad, int button);                  // Detect if a gamepad button has been released once
-- TODO    bool IsGamepadButtonUp(int gamepad, int button);                        // Detect if a gamepad button is NOT being pressed
-- TODO    int GetGamepadButtonPressed(void);                                      // Get the last gamepad button pressed
-- TODO    int GetGamepadAxisCount(int gamepad);                                   // Return gamepad axis count for a gamepad
-- TODO    float GetGamepadAxisMovement(int gamepad, int axis);                    // Return axis movement value for a gamepad axis
-- TODO
-- TODO    // Input-related functions: mouse
-- TODO    bool IsMouseButtonPressed(int button);                                  // Detect if a mouse button has been pressed once
-- TODO    bool IsMouseButtonDown(int button);                                     // Detect if a mouse button is being pressed
-- TODO    bool IsMouseButtonReleased(int button);                                 // Detect if a mouse button has been released once
-- TODO    bool IsMouseButtonUp(int button);                                       // Detect if a mouse button is NOT being pressed
-- TODO    int GetMouseX(void);                                                    // Returns mouse position X
-- TODO    int GetMouseY(void);                                                    // Returns mouse position Y
-- TODO    Vector2 GetMousePosition(void);                                         // Returns mouse position XY
-- TODO    void SetMousePosition(Vector2 position);                                // Set mouse position XY
-- TODO    int GetMouseWheelMove(void);                                            // Returns mouse wheel movement Y
-- TODO
-- TODO    // Input-related functions: touch
-- TODO    int GetTouchX(void);                                                    // Get touch position X for touch point 0 (relative to screen size)
-- TODO    int GetTouchY(void);                                                    // Get touch position Y for touch point 0 (relative to screen size)
-- TODO    Vector2 GetTouchPosition(int index);                                    // Get touch position XY for a touch point index (relative to screen size)
-- TODO
-- TODO    // Gestures-related functions
-- TODO    void SetGesturesEnabled(unsigned int gestureFlags);                     // Enable a set of gestures using flags
-- TODO    bool IsGestureDetected(int gesture);                                    // Check if a gesture have been detected
-- TODO    int GetGestureDetected(void);                                           // Get latest detected gesture
-- TODO    int GetTouchPointsCount(void);                                          // Get touch points count
-- TODO    float GetGestureHoldDuration(void);                                     // Get gesture hold time in milliseconds
-- TODO    Vector2 GetGestureDragVector(void);                                     // Get gesture drag vector
-- TODO    float GetGestureDragAngle(void);                                        // Get gesture drag angle
-- TODO    Vector2 GetGesturePinchVector(void);                                    // Get gesture pinch delta
-- TODO    float GetGesturePinchAngle(void);                                       // Get gesture pinch angle
-- TODO    
-- TODO    // Camera-related functions
-- TODO    void SetCameraMode(Camera camera, int mode);                            // Set camera mode (multiple camera modes available)
-- TODO    void UpdateCamera(Camera *camera);                                      // Update camera position for selected mode
-- TODO    void SetCameraPanControl(int panKey);                                   // Set camera pan key to combine with mouse movement (free camera)
-- TODO    void SetCameraAltControl(int altKey);                                   // Set camera alt key to combine with mouse movement (free camera)
-- TODO    void SetCameraSmoothZoomControl(int szKey);                             // Set camera smooth zoom key to combine with mouse (free camera)
-- TODO    void SetCameraMoveControls(int frontKey, int backKey,
-- TODO                               int rightKey, int leftKey,
-- TODO                               int upKey, int downKey);                     // Set camera move controls (1st person and 3rd person cameras)
