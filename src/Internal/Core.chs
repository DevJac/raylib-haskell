{-# LANGUAGE ForeignFunctionInterface #-}
module Internal.Core where
{# import Internal.Structs #} (Image, withImage)

#include "raylib.h"

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
