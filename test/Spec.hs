import GHC.Clock
import Colors
import Core
import Shapes
import Text
import Textures
import Types

drawForTime :: Double -> (Double -> IO ()) -> IO ()
drawForTime t action = do
  startTime <- getMonotonicTime
  loop startTime
  where
    loop startTime = do
      currentTime <- getMonotonicTime
      let timeRatio = (currentTime - startTime) / t
      clearBackground (Color 0 0 0 255)
      beginDrawing
      drawFPS 0 0
      action timeRatio
      endDrawing
      if timeRatio < 1 then loop startTime else pure ()

main :: IO ()
main = do
  setTraceLog []
  initWindow 400 400 "Tests"
  sequence_ $ drawForTime 4 <$> drawTests
  closeWindow

drawTests :: [Double -> IO ()]
drawTests = [ testDrawPixel
            , testDrawLine
            , testDrawCircle
            , testDrawSuccessMessage
            ]

testDrawSuccessMessage :: Double -> IO ()
testDrawSuccessMessage tr = do
  font <- getFontDefault
  drawText "Tests" 100 100 20 white
  testsSize <- measureText "Tests" 20
  drawTextEx font "Successful" (Vector2 100 140) 20 (realToFrac (20 * tr)) green
  successfulSize <- measureTextEx font "Successful" 20 (realToFrac (20 * tr))
  drawText (show testsSize) 100 300 20 grey
  drawText (show successfulSize) 100 350 20 grey

testDrawPixel :: Double -> IO ()
testDrawPixel tr = do
  drawPixel 100 (floor (50 + (300 * tr))) yellow
  drawPixelV (Vector2 300 350) purple

testDrawLine :: Double -> IO ()
testDrawLine tr = do
  drawLine 50 50 350 50 yellow
  drawLineV (Vector2 50 100) (Vector2 350 100) purple
  drawLineEx (Vector2 50 150) (Vector2 350 150) (realToFrac (20 * tr)) green
  drawLineBezier (Vector2 50 200) (Vector2 350 350) (realToFrac (20 * tr)) blue

testDrawCircle tr = do
  drawCircle 100 100 80 red
  drawCircleGradient 300 100 80 green blue
  drawCircleV (Vector2 100 300) 80 purple
  drawCircleLines 300 300 (realToFrac (80 * tr)) yellow



{-
main :: IO ()
main = do
  setTraceLog [Info, Warning, Error]
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
  drawText (show (fontCharsCount font)) 100 350 20 gold
  drawText (show (imageWidth image, imageHeight image)) 100 400 20 maroon
  endDrawing
  loop (n + 1)
-}
