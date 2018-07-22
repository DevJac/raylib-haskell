module Core (
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
) where
import qualified Internal.Core
import           Internal.Structs (Image)

-----------------------------------------
-- Window-related functions
-----------------------------------------

initWindow :: Int -> Int -> String -> IO ()
initWindow width height title = Internal.Core.initWindow width height title

closeWindow :: IO ()
closeWindow = Internal.Core.closeWindow

isWindowReady :: IO Bool
isWindowReady = Internal.Core.isWindowReady

windowShouldClose :: IO Bool
windowShouldClose = Internal.Core.windowShouldClose

isWindowMinimized :: IO Bool
isWindowMinimized = Internal.Core.isWindowMinimized

toggleFullscreen :: IO ()
toggleFullscreen = Internal.Core.toggleFullscreen

setWindowIcon :: Image -> IO ()
setWindowIcon image = Internal.Core.setWindowIcon image

setWindowTitle :: String -> IO ()
setWindowTitle title = Internal.Core.setWindowTitle title

setWindowPosition :: Int -> Int -> IO ()
setWindowPosition x y = Internal.Core.setWindowPosition x y

setWindowMonitor :: Int -> IO ()
setWindowMonitor monitor = Internal.Core.setWindowMonitor monitor

setWindowMinSize :: Int -> Int -> IO ()
setWindowMinSize width height = Internal.Core.setWindowMinSize width height

setWindowSize :: Int -> Int -> IO ()
setWindowSize width height = Internal.Core.setWindowSize width height

getScreenWidth :: IO Int
getScreenWidth = Internal.Core.getScreenWidth

getScreenHeight :: IO Int
getScreenHeight = Internal.Core.getScreenHeight
