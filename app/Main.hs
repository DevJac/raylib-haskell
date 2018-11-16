import Control.Concurrent
import Core
import Shapes
import Text
import Textures
import Types

main :: IO ()
main = do
  setTraceLog [Info, Warning, Error, Other]
  initWindow 500 500 "Hello World"
  print =<< isWindowReady
  icon <- loadImage "test_data/raylib_16x16.png"
  setWindowIcon icon
  loop 0

loop :: Int -> IO ()
loop n = do
  _image <- loadImage "test_data/raylib_16x16.png"
  font <- getFontDefault
  traceLog Info "My trace log!"
  print n
  width <- getScreenWidth
  height <- getScreenHeight
  clearBackground (Color 0 0 0 255)
  beginDrawing
  drawText (show (width, height)) 100 200 20 (Color 255 0 0 255)
  drawFPS 100 50
  let x = realToFrac $ n `div` 100 `mod` 400
      y = realToFrac $ n `mod` 400
  drawRectangleRec (Rectangle x y (x + 100) (y + 100)) (Color 120 120 255 255)
  drawTextEx font "Text is Test" (Vector2 100 100) 20 5 (Color 255 255 0 255)
  drawText "Test is Text" 100 150 20 (Color 255 128 0 255)
  endDrawing
  loop (n + 1)
