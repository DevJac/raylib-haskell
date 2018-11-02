{-# LANGUAGE ForeignFunctionInterface #-}
module Internal.Shapes where
{# import Internal.Types #} (
    Color(Color)
  , Vector2(Vector2)
  , Rectangle(Rectangle)
  )

#include "raylib.h"

{# fun unsafe DrawPixel as ^
    {`Int', `Int', %`Color'} -> `()' #}

{# fun unsafe DrawPixelV as ^
    {%`Vector2', %`Color'} -> `()' #}

{# fun unsafe DrawLine as ^
    {`Int', `Int', `Int', `Int', %`Color'} -> `()' #}

{# fun unsafe DrawLineV as ^
    {%`Vector2', %`Vector2', %`Color'} -> `()' #}

{# fun unsafe DrawLineEx as ^
    {%`Vector2', %`Vector2', `Float', %`Color'} -> `()' #}

{# fun unsafe DrawLineBezier as ^
    {%`Vector2', %`Vector2', `Float', %`Color'} -> `()' #}

{# fun unsafe DrawCircle as ^
    {`Int', `Int', `Float', %`Color'} -> `()' #}

{# fun unsafe DrawCircleGradient as ^
    {`Int', `Int', `Float', %`Color', %`Color'} -> `()' #}

{# fun unsafe DrawCircleV as ^
    {%`Vector2', `Float', %`Color'} -> `()' #}

{# fun unsafe DrawCircleLines as ^
    {`Int', `Int', `Float', %`Color'} -> `()' #}

{# fun unsafe DrawRectangle as ^
    {`Int', `Int', `Int', `Int', %`Color'} -> `()' #}

{# fun unsafe DrawRectangleV as ^
    {%`Vector2', %`Vector2', %`Color'} -> `()' #}

{# fun unsafe DrawRectangleRec as ^
    {%`Rectangle', %`Color'} -> `()' #}

{# fun unsafe DrawRectanglePro as ^
    {%`Rectangle', %`Vector2', `Float', %`Color'} -> `()' #}

{# fun unsafe DrawRectangleGradientV as ^
    {`Int', `Int', `Int', `Int', %`Color', %`Color'} -> `()' #}

{# fun unsafe DrawRectangleGradientH as ^
    {`Int', `Int', `Int', `Int', %`Color', %`Color'} -> `()' #}

{# fun unsafe DrawRectangleGradientEx as ^
    {%`Rectangle', %`Color', %`Color', %`Color', %`Color'} -> `()' #}

{# fun unsafe DrawRectangleLines as ^
    {`Int', `Int', `Int', `Int', %`Color'} -> `()' #}

{# fun unsafe DrawRectangleLinesEx as ^
    {%`Rectangle', `Int', %`Color'} -> `()' #}

{# fun unsafe DrawTriangle as ^
    {%`Vector2', %`Vector2', %`Vector2', %`Color'} -> `()' #}

{# fun unsafe DrawTriangleLines as ^
    {%`Vector2', %`Vector2', %`Vector2', %`Color'} -> `()' #}

{# fun unsafe DrawPoly as ^
    {%`Vector2', `Int', `Float', `Float', %`Color'} -> `()' #}

{# fun unsafe DrawPolyEx as ^
    {`Vector2', `Int', %`Color'} -> `()' #}

{# fun unsafe DrawPolyExLines as ^
    {`Vector2', `Int', %`Color'} -> `()' #}
