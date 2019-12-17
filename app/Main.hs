import Colors
import Core
import Shapes
import Text
import Textures
import Types

main :: IO ()
main = do
  setTraceLogLevel Info
  setConfigFlags [WindowResizable, Msaa4x]
  initWindow 500 500 "Hello World"
  print =<< isWindowReady
  icon <- loadImage "test_data/raylib_16x16.png"
  setWindowIcon icon
  loop 0

loop :: Int -> IO ()
loop n = do
  image <- loadImage "test_data/raylib_16x16.png"
  font <- getFontDefault
  traceLog Info "My trace log!"
  print n
  width <- getScreenWidth
  height <- getScreenHeight
  clearBackground (Color 0 0 0 255)
  beginDrawing
  drawPolyEx (reverse [Vector2 200 50, Vector2 250 75, Vector2 250 275, Vector2 200 300]) red
  drawText (show (width, height)) 100 200 20 (Color 255 0 0 255)
  drawFPS 100 50
  let x = realToFrac $ n `div` 100 `mod` 400
      y = realToFrac $ n `mod` 400
  drawRectangleRec (Rectangle x y (x + 100) (y + 100)) (Color 120 120 255 255)
  drawTextEx font "Text is Test" (Vector2 100 100) 20 5 (Color 255 255 0 255)
  mousePosition <- getMousePosition
  drawText "Test is Text" 100 150 20 (Color 255 128 0 255)
  drawText (show mousePosition) 100 250 20 pink
  textMeasure <- measureTextEx font "Text is Test" 20 5
  drawText (show textMeasure) 100 300 20 green
  charsCount <- fontCharsCount font
  drawText (show (charsCount)) 100 350 20 gold
  imageW <- imageWidth image
  imageH <- imageHeight image
  drawText (show (imageW, imageH)) 100 400 20 maroon
  endDrawing
  loop (n + 1)
