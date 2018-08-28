{-# LANGUAGE ForeignFunctionInterface #-}
module Internal.Core where
import Data.Word (Word8)
{# import Internal.Types #} (
    Color(Color)
  , Vector2(Vector2)
  , Vector3(Vector3)
  , Vector4(Vector4)
  , Matrix(Matrix)
  , Image, withImage
  , RenderTexture2D, withRenderTexture2D
  , Camera3D(Camera3D)
  , Camera2D(Camera2D)
  , Ray(Ray)
  )

#include "raylib.h"
#include "core_wrapper.h"

-----------------------------------------
-- Window related functions
-----------------------------------------

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

-----------------------------------------
-- Cursor related functions
-----------------------------------------

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

-----------------------------------------
-- Drawing related functions
-----------------------------------------

{# fun unsafe ClearBackground as ^
    {%`Color'} -> `()' #}

{# fun unsafe BeginDrawing as ^
    {} -> `()' #}

{# fun unsafe EndDrawing as ^
    {} -> `()' #}

{# fun unsafe BeginMode2D as ^
    {%`Camera2D'} -> `()' #}

{# fun unsafe EndMode2D as ^
    {} -> `()' #}

{# fun unsafe BeginMode3D as ^
    {%`Camera3D'} -> `()' #}

{# fun unsafe EndMode3D as ^
    {} -> `()' #}

{# fun unsafe BeginTextureMode as ^
    {%`RenderTexture2D'} -> `()' #}

{# fun unsafe EndTextureMode as ^
    {} -> `()' #}

-----------------------------------------
-- Screen-space related functions
-----------------------------------------

{# fun unsafe WrappedGetMouseRay as getMouseRay
    {%`Vector2', %`Camera3D'} -> `Ray' #}

{# fun unsafe WrappedGetWorldToScreen as getWorldToScreen
    {%`Vector3', %`Camera3D'} -> `Vector2' #}

{# fun unsafe WrappedGetCameraMatrix as getCameraMatrix
    {%`Camera3D'} -> `Matrix' #}

-----------------------------------------
-- Timing related functions
-----------------------------------------

{# fun unsafe SetTargetFPS as ^
    {`Int'} -> `()' #}

{# fun unsafe GetFPS as ^
    {} -> `Int' #}

{# fun unsafe GetFrameTime as ^
    {} -> `Float' #}

{# fun unsafe GetTime as ^
    {} -> `Double' #}

-----------------------------------------
-- Color related functions
-----------------------------------------

{# fun unsafe ColorToInt as ^
    {%`Color'} -> `Int' #}

{# fun unsafe WrappedColorNormalize as colorNormalize
    {%`Color'} -> `Vector4' #}

{# fun unsafe WrappedColorToHSV as colorToHSV
    {%`Color'} -> `Vector3' #}

{# fun unsafe WrappedGetColor as getColor
    {%`Int'} -> `Color' #}

{# fun unsafe WrappedFade as fade
    {%`Color', %`Float'} -> `Color' #}

-----------------------------------------
-- Misc. functions
-----------------------------------------

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

{# fun unsafe GetRandomValue as ^
    {`Int', `Int'} -> `Int' #}

-----------------------------------------
-- Input related functions: keyboard
-----------------------------------------

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

-----------------------------------------
-- Input related functions: mouse
-----------------------------------------

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
    {} -> `Vector2' #}

{# fun unsafe SetMousePosition as ^
    {%`Vector2'} -> `()' #}

{# fun unsafe GetMouseWheelMove as ^
    {} -> `Int' #}

-----------------------------------------
-- Camera related functions
-----------------------------------------

{# fun unsafe SetCameraMode as ^
    {%`Camera3D', `Int'} -> `()' #}

{# fun unsafe WrappedUpdateCamera as updateCamera
    {`Camera3D'} -> `Camera3D' #}

{# fun unsafe SetCameraPanControl as ^
    {`Int'} -> `()' #}

{# fun unsafe SetCameraAltControl as ^
    {`Int'} -> `()' #}

{# fun unsafe SetCameraSmoothZoomControl as ^
    {`Int'} -> `()' #}

{# fun unsafe SetCameraMoveControls as ^
    {`Int', `Int', `Int', `Int', `Int', `Int'} -> `()' #}
