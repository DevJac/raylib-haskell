{-# LANGUAGE ForeignFunctionInterface #-}
module Internal.Core where
{# import Internal.Structs #} (
    Color(Color)
  , Vector2(Vector2)
  , Vector3(Vector3)
  , Matrix(Matrix)
  , Image, withImage
  , RenderTexture2D, withRenderTexture2D
  , Camera3D(Camera3D)
  , Camera2D(Camera2D)
  , Ray(Ray)
  )

#include "raylib.h"
#include "core_wrapper.h"

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

{# fun unsafe WrappedGetMouseRay as getMouseRay
    {%`Vector2', %`Camera3D'} -> `Ray' #}

{# fun unsafe WrappedGetWorldToScreen as getWorldToScreen
    {%`Vector3', %`Camera3D'} -> `Vector2' #}

{# fun unsafe WrappedGetCameraMatrix as getCameraMatrix
    {%`Camera3D'} -> `Matrix' #}
