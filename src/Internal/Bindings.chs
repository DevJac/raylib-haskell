{- We will use a single c2hs file for this entire project. This is due to a couple of c2hs bugs:

   - {# import ... #} breaks implicit c2hs imports, which makes using multiple c2hs modules hard.
     See: https://github.com/haskell/c2hs/issues/189

   - The finalizers defined by {# pointer ... finalizer ... #} do not work across modules.
     See: https://github.com/haskell/c2hs/issues/174
-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Internal.Bindings where
import Data.Coerce
import Data.Word
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

#include "raylib.h"
#include "cbits.h"

---------------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------------

-- | @Color r g b a@
data Color = Color { colorR :: !Word8 -- ^ red channel
                   , colorG :: !Word8 -- ^ blue channel
                   , colorB :: !Word8 -- ^ green channel
                   , colorA :: !Word8 -- ^ alpha channel
                   } deriving (Show, Eq)

{# pointer *Color as ColorPtr -> Color #}

instance Storable Color where
  sizeOf _ = {# sizeof Color #}
  alignment _ = {# alignof Color #}
  peek p = do
    r <- fromIntegral <$> {# get Color.r #} p
    g <- fromIntegral <$> {# get Color.g #} p
    b <- fromIntegral <$> {# get Color.b #} p
    a <- fromIntegral <$> {# get Color.a #} p
    pure $ Color r g b a
  poke p (Color r g b a) = do
    {# set Color.r #} p (fromIntegral r)
    {# set Color.g #} p (fromIntegral g)
    {# set Color.b #} p (fromIntegral b)
    {# set Color.a #} p (fromIntegral a)

-- | @Rectangle x y width height@
data Rectangle = Rectangle { rectangleX :: !Float -- ^ X coordinate
                           , rectangleY :: !Float -- ^ Y coordinate
                           , rectangleWidth :: !Float -- ^ width
                           , rectangleHeight :: !Float -- ^ height
                           } deriving (Show, Eq)

{# pointer *Rectangle as RectanglePtr -> Rectangle #}

instance Storable Rectangle where
  sizeOf _ = {# sizeof Rectangle #}
  alignment _ = {# alignof Rectangle #}
  peek p = do
    x      <- realToFrac <$> {# get Rectangle.x #}      p
    y      <- realToFrac <$> {# get Rectangle.y #}      p
    width  <- realToFrac <$> {# get Rectangle.width #}  p
    height <- realToFrac <$> {# get Rectangle.height #} p
    pure $ Rectangle x y width height
  poke p (Rectangle x y width height) = do
    {# set Rectangle.x #}      p (realToFrac x)
    {# set Rectangle.y #}      p (realToFrac y)
    {# set Rectangle.width #}  p (realToFrac width)
    {# set Rectangle.height #} p (realToFrac height)

-- | @Vector2 x y@
data Vector2 = Vector2 { vector2X :: !Float -- ^ X coordinate
                       , vector2Y :: !Float -- ^ Y coordinate
                       } deriving (Show, Eq)

{# pointer *Vector2 as Vector2Ptr -> Vector2 #}

instance Storable Vector2 where
  sizeOf _ = {# sizeof Vector2 #}
  alignment _ = {# alignof Vector2 #}
  peek p = do
    x <- realToFrac <$> {# get Vector2.x #} p
    y <- realToFrac <$> {# get Vector2.y #} p
    pure $ Vector2 x y
  poke p (Vector2 x y) = do
    {# set Vector2.x #} p (realToFrac x)
    {# set Vector2.y #} p (realToFrac y)

-- | @Vector3 x y z@
data Vector3 = Vector3 { vector3X :: !Float -- ^ X coordinate
                       , vector3Y :: !Float -- ^ Y coordinate
                       , vector3Z :: !Float -- ^ Z coordinate
                       } deriving (Show, Eq)

{# pointer *Vector3 as Vector3Ptr -> Vector3 #}

instance Storable Vector3 where
  sizeOf _ = {# sizeof Vector3 #}
  alignment _ = {# alignof Vector3 #}
  peek p = do
    x <- realToFrac <$> {# get Vector3.x #} p
    y <- realToFrac <$> {# get Vector3.y #} p
    z <- realToFrac <$> {# get Vector3.z #} p
    pure $ Vector3 x y z
  poke p (Vector3 x y z) = do
    {# set Vector3.x #} p (realToFrac x)
    {# set Vector3.y #} p (realToFrac y)
    {# set Vector3.z #} p (realToFrac z)

-- | @Vector4 x y z w@
--
--   This is also known as a 'Quaternion' in raylib.
data Vector4 = Vector4 { vector4X :: !Float -- ^ X coordinate
                       , vector4Y :: !Float -- ^ Y coordinate
                       , vector4Z :: !Float -- ^ Z coordinate
                       , vector4W :: !Float -- ^ W coordinate
                       } deriving (Show, Eq)

type Quaternion = Vector4

{# pointer *Vector4 as Vector4Ptr -> Vector4 #}

instance Storable Vector4 where
  sizeOf _ = {# sizeof Vector4 #}
  alignment _ = {# alignof Vector4 #}
  peek p = do
    x <- realToFrac <$> {# get Vector4.x #} p
    y <- realToFrac <$> {# get Vector4.y #} p
    z <- realToFrac <$> {# get Vector4.z #} p
    w <- realToFrac <$> {# get Vector4.w #} p
    pure $ Vector4 x y z w
  poke p (Vector4 x y z w) = do
    {# set Vector4.x #} p (realToFrac x)
    {# set Vector4.y #} p (realToFrac y)
    {# set Vector4.z #} p (realToFrac z)
    {# set Vector4.w #} p (realToFrac w)

-- | @Matrix m0 m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 m13 m14 m15@
--
--   Note the field accessors follow a naming scheme inspired by __zero-based__ indexing. @matrix01@ accesses the value on the \"zeroth\" row and first column.
data Matrix = Matrix { matrix00 :: !Float -- ^ \"zeroth\" row, \"zeroth\" column
                     , matrix01 :: !Float
                     , matrix02 :: !Float
                     , matrix03 :: !Float
                     , matrix10 :: !Float
                     , matrix11 :: !Float
                     , matrix12 :: !Float -- ^ first row, second column
                     , matrix13 :: !Float
                     , matrix20 :: !Float
                     , matrix21 :: !Float
                     , matrix22 :: !Float
                     , matrix23 :: !Float
                     , matrix30 :: !Float
                     , matrix31 :: !Float -- ^ third row, first column
                     , matrix32 :: !Float
                     , matrix33 :: !Float
                     } deriving (Show, Eq)

{# pointer *Matrix as MatrixPtr -> Matrix #}

instance Storable Matrix where
  sizeOf _ = {# sizeof Matrix #}
  alignment _ = {# alignof Matrix #}
  peek p = do
    m0  <- realToFrac <$> {# get Matrix.m0  #} p
    m1  <- realToFrac <$> {# get Matrix.m1  #} p
    m2  <- realToFrac <$> {# get Matrix.m2  #} p
    m3  <- realToFrac <$> {# get Matrix.m3  #} p
    m4  <- realToFrac <$> {# get Matrix.m4  #} p
    m5  <- realToFrac <$> {# get Matrix.m5  #} p
    m6  <- realToFrac <$> {# get Matrix.m6  #} p
    m7  <- realToFrac <$> {# get Matrix.m7  #} p
    m8  <- realToFrac <$> {# get Matrix.m8  #} p
    m9  <- realToFrac <$> {# get Matrix.m9  #} p
    m10 <- realToFrac <$> {# get Matrix.m10 #} p
    m11 <- realToFrac <$> {# get Matrix.m11 #} p
    m12 <- realToFrac <$> {# get Matrix.m12 #} p
    m13 <- realToFrac <$> {# get Matrix.m13 #} p
    m14 <- realToFrac <$> {# get Matrix.m14 #} p
    m15 <- realToFrac <$> {# get Matrix.m15 #} p
    pure $ Matrix m0  m1  m2  m3
                  m4  m5  m6  m7
                  m8  m9  m10 m11
                  m12 m13 m14 m15
  poke p (Matrix m0  m1  m2  m3
                 m4  m5  m6  m7
                 m8  m9  m10 m11
                 m12 m13 m14 m15) = do
    {# set Matrix.m0  #} p (realToFrac m0 )
    {# set Matrix.m1  #} p (realToFrac m1 )
    {# set Matrix.m2  #} p (realToFrac m2 )
    {# set Matrix.m3  #} p (realToFrac m3 )
    {# set Matrix.m4  #} p (realToFrac m4 )
    {# set Matrix.m5  #} p (realToFrac m5 )
    {# set Matrix.m6  #} p (realToFrac m6 )
    {# set Matrix.m7  #} p (realToFrac m7 )
    {# set Matrix.m8  #} p (realToFrac m8 )
    {# set Matrix.m9  #} p (realToFrac m9 )
    {# set Matrix.m10 #} p (realToFrac m10)
    {# set Matrix.m11 #} p (realToFrac m11)
    {# set Matrix.m12 #} p (realToFrac m12)
    {# set Matrix.m13 #} p (realToFrac m13)
    {# set Matrix.m14 #} p (realToFrac m14)
    {# set Matrix.m15 #} p (realToFrac m15)

-- | Camera3D position target up fovy type
data Camera3D = Camera3D !Vector3 !Vector3 !Vector3 !Float !Int deriving (Show, Eq)

{# pointer *Camera3D as Camera3DPtr -> Camera3D #}

instance Storable Camera3D where
  sizeOf _ = {# sizeof Camera3D #}
  alignment _ = {# alignof Camera3D #}
  peek p = do
    position <- peek $ p `plusPtr` {# offsetof Camera3D.position #}
    target   <- peek $ p `plusPtr` {# offsetof Camera3D.target #}
    up       <- peek $ p `plusPtr` {# offsetof Camera3D.up #}
    fovy     <- realToFrac   <$> {# get Camera3D.fovy #} p
    type_    <- fromIntegral <$> {# get Camera3D.type #} p
    pure $ Camera3D position target up fovy type_
  poke p (Camera3D position target up fovy type_) = do
    poke (p `plusPtr` {# offsetof Camera3D.position #}) position
    poke (p `plusPtr` {# offsetof Camera3D.target #})   target
    poke (p `plusPtr` {# offsetof Camera3D.up #})       up
    {# set Camera3D.fovy #} p (realToFrac fovy)
    {# set Camera3D.type #} p (fromIntegral type_)

-- | Camera2D offset target rotation zoom
data Camera2D = Camera2D !Vector2 !Vector2 !Float !Float deriving (Show, Eq)

{# pointer *Camera2D as Camera2DPtr -> Camera2D #}

instance Storable Camera2D where
  sizeOf _ = {# sizeof Camera2D #}
  alignment _ = {# alignof Camera2D #}
  peek p = do
    offset   <- peek $ p `plusPtr` {# offsetof Camera2D.offset #}
    target   <- peek $ p `plusPtr` {# offsetof Camera2D.target #}
    rotation <- realToFrac <$> {# get Camera2D.rotation #} p
    zoom     <- realToFrac <$> {# get Camera2D.zoom #}     p
    pure $ Camera2D offset target rotation zoom
  poke p (Camera2D offset target rotation zoom) = do
    poke (p `plusPtr` {# offsetof Camera2D.offset #}) offset
    poke (p `plusPtr` {# offsetof Camera2D.target #}) target
    {# set Camera2D.rotation #} p (realToFrac rotation)
    {# set Camera2D.zoom #}     p (realToFrac zoom)

-- | Ray position direction
data Ray = Ray !Vector3 !Vector3 deriving (Show, Eq)

{# pointer *Ray as RayPtr -> Ray #}

instance Storable Ray where
  sizeOf _ = {# sizeof Ray #}
  alignment _ = {# alignof Ray #}
  peek p = do
    position  <- peek $ p `plusPtr` {# offsetof Ray.position #}
    direction <- peek $ p `plusPtr` {# offsetof Ray.direction #}
    pure $ Ray position direction
  poke p (Ray position direction) = do
    poke (p `plusPtr` {# offsetof Ray.position #})  position
    poke (p `plusPtr` {# offsetof Ray.direction #}) direction

-- | RayHitInfo hit distance position normal
data RayHitInfo = RayHitInfo !Bool !Float !Vector3 !Vector3 deriving (Show, Eq)

{# pointer *RayHitInfo as RayHitInfoPtr -> RayHitInfo #}

instance Storable RayHitInfo where
  sizeOf _ = {# sizeof RayHitInfo #}
  alignment _ = {# sizeof RayHitInfo #}
  peek p = do
    hit      <- toBool     <$> {# get RayHitInfo.hit #}      p
    distance <- realToFrac <$> {# get RayHitInfo.distance #} p
    position <- peek $ p `plusPtr` {# offsetof RayHitInfo.position #}
    normal   <- peek $ p `plusPtr` {# offsetof RayHitInfo.normal #}
    pure $ RayHitInfo hit distance position normal
  poke p (RayHitInfo hit distance position normal) = do
    {# set RayHitInfo.hit #}      p (fromBool hit)
    {# set RayHitInfo.distance #} p (realToFrac distance)
    poke (p `plusPtr` {# offsetof RayHitInfo.position #}) position
    poke (p `plusPtr` {# offsetof RayHitInfo.normal #})   normal

-- | Image
{# pointer *Image foreign finalizer WrappedUnloadImage as unloadImage newtype #}

-- | Texture2D
{# pointer *Texture2D foreign finalizer WrappedUnloadTexture as unloadTexture newtype #}

-- | RenderTexture2D
{# pointer *RenderTexture2D foreign finalizer WrappedUnloadRenderTexture as unloadRenderTexture newtype #}

-- | Font
{# pointer *Font foreign finalizer WrappedUnloadFont as unloadFont newtype #}

-- | Model
{# pointer *Model foreign finalizer WrappedUnloadModel as unloadModel newtype #}

-- | Mesh

-- raylib's UnloadMesh function takes a pointer, so we don't have to wrap it like the other Unload* functions.
{# pointer *Mesh foreign finalizer UnloadMesh as unloadMesh newtype #}

-- | Material
{# pointer *Material foreign finalizer WrappedUnloadMaterial as unloadMaterial newtype #}

-- | Shader
{# pointer *Shader foreign finalizer WrappedUnloadShader as unloadShader newtype #}

-- | Wave
{# pointer *Wave foreign finalizer WrappedUnloadWave as unloadWave newtype #}

-- | Sound
{# pointer *Sound foreign finalizer WrappedUnloadSound as unloadSound newtype #}

-- | Music
{# pointer *Music foreign finalizer WrappedUnloadMusicStream as unloadMusicStream newtype #}

-- | AudioStream
{# pointer *AudioStream foreign finalizer WrappedCloseAudioStream as closeAudioStream newtype #}

---------------------------------------------------------------------------------
-- Core
---------------------------------------------------------------------------------

{# fun unsafe InitWindow as ^
  {`Int', `Int', `String'} -> `()' #}

{# fun unsafe CloseWindow as ^
  {} -> `()' #}

{# fun unsafe IsWindowReady as ^
  {} -> `Bool' #}

{# fun unsafe WindowShouldClose as ^
  {} -> `Bool' #}

{# fun unsafe IsWindowMinimized as ^
  {} -> `Bool' #}

{# fun unsafe ToggleFullscreen as ^
  {} -> `()' #}

{# fun unsafe SetWindowIcon as ^
  {%`Image'} -> `()' #}

{# fun unsafe SetWindowTitle as ^
  {`String'} -> `()' #}

{# fun unsafe SetWindowPosition as ^
  {`Int', `Int'} -> `()' #}

{# fun unsafe SetWindowMonitor as ^
  {`Int'} -> `()' #}

{# fun unsafe SetWindowMinSize as ^
  {`Int', `Int'} -> `()' #}

{# fun unsafe SetWindowSize as ^
  {`Int', `Int'} -> `()' #}

{# fun unsafe GetScreenWidth as ^
  {} -> `Int' #}

{# fun unsafe GetScreenHeight as ^
  {} -> `Int' #}

{# fun unsafe ShowCursor as ^
  {} -> `()' #}

{# fun unsafe HideCursor as ^
  {} -> `()' #}

{# fun unsafe IsCursorHidden as ^
  {} -> `Bool' #}

{# fun unsafe EnableCursor as ^
  {} -> `()' #}

{# fun unsafe DisableCursor as ^
  {} -> `()' #}

{# fun unsafe ClearBackground as ^
  {%`ColorPtr'} -> `()' #}

{# fun unsafe BeginDrawing as ^
  {} -> `()' #}

{# fun unsafe EndDrawing as ^
  {} -> `()' #}

{# fun unsafe BeginMode2D as ^
  {%`Camera2DPtr'} -> `()' #}

{# fun unsafe EndMode2D as ^
  {} -> `()' #}

{# fun unsafe BeginMode3D as ^
  {%`Camera3DPtr'} -> `()' #}

{# fun unsafe EndMode3D as ^
  {} -> `()' #}

{# fun unsafe BeginTextureMode as ^
  {%`RenderTexture2D'} -> `()' #}

{# fun unsafe EndTextureMode as ^
  {} -> `()' #}

{# fun unsafe WrappedGetMouseRay as getMouseRay
  {%`Vector2Ptr', %`Camera3DPtr', alloca- `Ray' peek*} -> `()' #}

{# fun unsafe WrappedGetWorldToScreen as getWorldToScreen
  {%`Vector3Ptr', %`Camera3DPtr', alloca- `Vector2' peek*} -> `()' #}

{# fun unsafe WrappedGetCameraMatrix as getCameraMatrix
  {%`Camera3DPtr', alloca- `Matrix' peek*} -> `()' #}

{# fun unsafe SetTargetFPS as ^
  {`Int'} -> `()' #}

{# fun unsafe GetFPS as ^
  {} -> `Int' #}

{# fun unsafe GetFrameTime as ^
  {} -> `Float' #}

{# fun unsafe GetTime as ^
  {} -> `Double' #}

{# fun unsafe ShowLogo as ^
  {} -> `()' #}

{# fun unsafe SetConfigFlags as ^
  {`Word8'} -> `()' #}

{# fun unsafe SetTraceLog as ^
  {`Word8'} -> `()' #}

{# fun unsafe TraceLog as ^
  {`Int', `String'} -> `()' #}

{# fun unsafe TakeScreenshot as ^
  {`String'} -> `()' #}

-- TODO // Files management functions
-- TODO const char *GetWorkingDirectory(void);                                  // Get current working directory (uses static string)
-- TODO bool ChangeDirectory(const char *dir);                                  // Change working directory, returns true if success
-- TODO bool IsFileDropped(void);                                               // Check if a file has been dropped into window
-- TODO char **GetDroppedFiles(int *count);                                     // Get dropped files names
-- TODO void ClearDroppedFiles(void);                                           // Clear dropped files paths buffer

{# fun unsafe IsKeyPressed as ^
  {`Int'} -> `Bool' #}

{# fun unsafe IsKeyDown as ^
  {`Int'} -> `Bool' #}

{# fun unsafe IsKeyReleased as ^
  {`Int'} -> `Bool' #}

{# fun unsafe IsKeyUp as ^
  {`Int'} -> `Bool' #}

{# fun unsafe GetKeyPressed as ^
  {} -> `Int' #}

{# fun unsafe SetExitKey as ^
  {`Int'} -> `()' #}

-- TODO // Input-related functions: gamepads
-- TODO bool IsGamepadAvailable(int gamepad);                                   // Detect if a gamepad is available
-- TODO bool IsGamepadName(int gamepad, const char *name);                      // Check gamepad name (if available)
-- TODO const char *GetGamepadName(int gamepad);                                // Return gamepad internal name id
-- TODO bool IsGamepadButtonPressed(int gamepad, int button);                   // Detect if a gamepad button has been pressed once
-- TODO bool IsGamepadButtonDown(int gamepad, int button);                      // Detect if a gamepad button is being pressed
-- TODO bool IsGamepadButtonReleased(int gamepad, int button);                  // Detect if a gamepad button has been released once
-- TODO bool IsGamepadButtonUp(int gamepad, int button);                        // Detect if a gamepad button is NOT being pressed
-- TODO int GetGamepadButtonPressed(void);                                      // Get the last gamepad button pressed
-- TODO int GetGamepadAxisCount(int gamepad);                                   // Return gamepad axis count for a gamepad
-- TODO float GetGamepadAxisMovement(int gamepad, int axis);                    // Return axis movement value for a gamepad axis

{# fun unsafe IsMouseButtonPressed as ^
  {`Int'} -> `Bool' #}

{# fun unsafe IsMouseButtonDown as ^
  {`Int'} -> `Bool' #}

{# fun unsafe IsMouseButtonReleased as ^
  {`Int'} -> `Bool' #}

{# fun unsafe IsMouseButtonUp as ^
  {`Int'} -> `Bool' #}

{# fun unsafe GetMouseX as ^
  {} -> `Int' #}

{# fun unsafe GetMouseY as ^
  {} -> `Int' #}

{# fun unsafe WrappedGetMousePosition as getMousePosition
  {alloca- `Vector2' peek*} -> `()' #}

{# fun unsafe SetMousePosition as ^
  {%`Vector2Ptr'} -> `()' #}

{# fun unsafe GetMouseWheelMove as ^
  {} -> `Int' #}

-- TODO // Input-related functions: touch
-- TODO int GetTouchX(void);                                                    // Get touch position X for touch point 0 (relative to screen size)
-- TODO int GetTouchY(void);                                                    // Get touch position Y for touch point 0 (relative to screen size)
-- TODO Vector2 GetTouchPosition(int index);                                    // Get touch position XY for a touch point index (relative to screen size)

-- TODO // Gestures-related functions
-- TODO void SetGesturesEnabled(unsigned int gestureFlags);                     // Enable a set of gestures using flags
-- TODO bool IsGestureDetected(int gesture);                                    // Check if a gesture have been detected
-- TODO int GetGestureDetected(void);                                           // Get latest detected gesture
-- TODO int GetTouchPointsCount(void);                                          // Get touch points count
-- TODO float GetGestureHoldDuration(void);                                     // Get gesture hold time in milliseconds
-- TODO Vector2 GetGestureDragVector(void);                                     // Get gesture drag vector
-- TODO float GetGestureDragAngle(void);                                        // Get gesture drag angle
-- TODO Vector2 GetGesturePinchVector(void);                                    // Get gesture pinch delta
-- TODO float GetGesturePinchAngle(void);                                       // Get gesture pinch angle

{# fun unsafe SetCameraMode as ^
  {%`Camera3DPtr', `Int'} -> `()' #}

{# fun unsafe UpdateCamera as ^
  {`Camera3DPtr'} -> `()' #}

{# fun unsafe SetCameraPanControl as ^
  {`Int'} -> `()' #}

{# fun unsafe SetCameraAltControl as ^
  {`Int'} -> `()' #}

{# fun unsafe SetCameraSmoothZoomControl as ^
  {`Int'} -> `()' #}

{# fun unsafe SetCameraMoveControls as ^
  {`Int', `Int', `Int', `Int', `Int', `Int'} -> `()' #}

---------------------------------------------------------------------------------
-- Shapes
---------------------------------------------------------------------------------

{# fun unsafe DrawPixel as ^
  {`Int', `Int', %`ColorPtr'} -> `()' #}

{# fun unsafe DrawPixelV as ^
  {%`Vector2Ptr', %`ColorPtr'} -> `()' #}

{# fun unsafe DrawLine as ^
  {`Int', `Int', `Int', `Int', %`ColorPtr'} -> `()' #}

{# fun unsafe DrawLineV as ^
  {%`Vector2Ptr', %`Vector2Ptr', %`ColorPtr'} -> `()' #}

{# fun unsafe DrawLineEx as ^
  {%`Vector2Ptr', %`Vector2Ptr', `Float', %`ColorPtr'} -> `()' #}

{# fun unsafe DrawLineBezier as ^
  {%`Vector2Ptr', %`Vector2Ptr', `Float', %`ColorPtr'} -> `()' #}

{# fun unsafe DrawCircle as ^
  {`Int', `Int', `Float', %`ColorPtr'} -> `()' #}

{# fun unsafe DrawCircleGradient as ^
  {`Int', `Int', `Float', %`ColorPtr', %`ColorPtr'} -> `()' #}

{# fun unsafe DrawCircleV as ^
  {%`Vector2Ptr', `Float', %`ColorPtr'} -> `()' #}

{# fun unsafe DrawCircleLines as ^
  {`Int', `Int', `Float', %`ColorPtr'} -> `()' #}

{# fun unsafe DrawRectangle as ^
  {`Int', `Int', `Int', `Int', %`ColorPtr'} -> `()' #}

{# fun unsafe DrawRectangleV as ^
  {%`Vector2Ptr', %`Vector2Ptr', %`ColorPtr'} -> `()' #}

{# fun unsafe DrawRectangleRec as ^
  {%`RectanglePtr', %`ColorPtr'} -> `()' #}

{# fun unsafe DrawRectanglePro as ^
  {%`RectanglePtr', %`Vector2Ptr', `Float', %`ColorPtr'} -> `()' #}

{# fun unsafe DrawRectangleGradientV as ^
  {`Int', `Int', `Int', `Int', %`ColorPtr', %`ColorPtr'} -> `()' #}

{# fun unsafe DrawRectangleGradientH as ^
  {`Int', `Int', `Int', `Int', %`ColorPtr', %`ColorPtr'} -> `()' #}

{# fun unsafe DrawRectangleGradientEx as ^
  {%`RectanglePtr', %`ColorPtr', %`ColorPtr', %`ColorPtr', %`ColorPtr'} -> `()' #}

{# fun unsafe DrawRectangleLines as ^
  {`Int', `Int', `Int', `Int', %`ColorPtr'} -> `()' #}

{# fun unsafe DrawRectangleLinesEx as ^
  {%`RectanglePtr', `Int', %`ColorPtr'} -> `()' #}

{# fun unsafe DrawTriangle as ^
  {%`Vector2Ptr', %`Vector2Ptr', %`Vector2Ptr', %`ColorPtr'} -> `()' #}

{# fun unsafe DrawTriangleLines as ^
  {%`Vector2Ptr', %`Vector2Ptr', %`Vector2Ptr', %`ColorPtr'} -> `()' #}

{# fun unsafe DrawPoly as ^
  {%`Vector2Ptr', `Int', `Float', `Float', %`ColorPtr'} -> `()' #}

{# fun unsafe DrawPolyEx as ^
  {`Vector2Ptr', `Int', %`ColorPtr'} -> `()' #}

{# fun unsafe DrawPolyExLines as ^
  {`Vector2Ptr', `Int', %`ColorPtr'} -> `()' #}

{# fun unsafe CheckCollisionRecs as ^
  {%`RectanglePtr', %`RectanglePtr'} -> `Bool' #}

{# fun unsafe CheckCollisionCircles as ^
  {%`Vector2Ptr', `Float', %`Vector2Ptr', `Float'} -> `Bool' #}

{# fun unsafe CheckCollisionCircleRec as ^
  {%`Vector2Ptr', `Float', %`RectanglePtr'} -> `Bool' #}

{# fun unsafe WrappedGetCollisionRec as getCollisionRec
  {%`RectanglePtr', %`RectanglePtr', alloca- `Rectangle' peek*} -> `()' #}

{# fun unsafe CheckCollisionPointRec as ^
  {%`Vector2Ptr', %`RectanglePtr'} -> `Bool' #}

{# fun unsafe CheckCollisionPointCircle as ^
  {%`Vector2Ptr', %`Vector2Ptr', `Float'} -> `Bool' #}

{# fun unsafe CheckCollisionPointTriangle as ^
  {%`Vector2Ptr', %`Vector2Ptr', %`Vector2Ptr', %`Vector2Ptr'} -> `Bool' #}
