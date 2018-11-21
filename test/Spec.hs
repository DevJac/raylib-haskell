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

sleep :: Double -> IO ()
sleep t = drawForTime t (\_ -> pure ())

main :: IO ()
main = do
  let windowWidth = 400
      windowHeight = 400
  setTraceLog [Warning, Error]
  setConfigFlags [ShowLogo, Msaa4x, Vsync]
  initWindow windowWidth windowHeight "Tests"
  setWindowIcon =<< loadImage "test_data/raylib_16x16.png"
  sleep 2
  toggleFullscreen
  sleep 2
  toggleFullscreen
  setWindowSize windowWidth windowHeight
  screenWidth <- getScreenWidth
  screenHeight <- getScreenHeight
  setWindowPosition ((screenWidth `div` 2) - (windowWidth `div` 2)) ((screenHeight `div` 2) - (windowHeight `div` 2))
  sequence_ $ drawForTime 4 <$> drawTests
  testInput
  drawForTime 4 testDrawSuccessMessage
  closeWindow

drawTests :: [Double -> IO ()]
drawTests = [ testDrawPixel
            , testDrawLine
            , testDrawCircle
            , testDrawRectangle
            , testDrawTriangle
            , testDrawPoly
            ]

testInput :: IO ()
testInput = do
  startTime <- getMonotonicTime
  loop startTime =<< getKeyPressed
  where
    loop startTime lastKey = do
      clearBackground (Color 0 0 0 255)
      beginDrawing
      drawFPS 0 0
      drawText "Last key pressed: " 100 100 20 grey
      drawText "Mouse position: " 100 200 20 grey
      lastKeyPressed <- getKeyPressed
      drawText (show lastKey) 100 150 20 yellow
      mousePosition <- getMousePosition
      drawText (show mousePosition) 100 250 20 orange
      currentTime <- getMonotonicTime
      drawText (show (ceiling (20 - (currentTime - startTime)) :: Int)) 50 50 20 grey
      endDrawing
      if currentTime - startTime > 20 then pure () else loop startTime (if lastKeyPressed /= NoKey then lastKeyPressed else lastKey)


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

testDrawCircle :: Double -> IO ()
testDrawCircle tr = do
  drawCircle 100 100 80 red
  drawCircleGradient 300 100 80 green blue
  drawCircleV (Vector2 100 300) 80 purple
  drawCircleLines 300 300 (realToFrac (80 * tr)) yellow

testDrawRectangle :: Double -> IO ()
testDrawRectangle tr = do
  drawRectangle 5 5 40 40 white
  drawRectangleV (Vector2 55 5) (Vector2 40 40) grey
  drawRectangleRec (Rectangle 105 5 40 40) blue
  drawRectangleGradientV 155 5 40 40 purple yellow
  drawRectangleGradientH 205 5 40 40 red maroon
  drawRectangleGradientEx (Rectangle 255 5 40 40) red yellow green blue
  drawRectangleLines 5 55 40 40 pink
  drawRectangleLinesEx (Rectangle 55 55 40 40) (ceiling (20 * tr)) green

testDrawTriangle :: Double -> IO ()
testDrawTriangle tr = do
  drawTriangle (Vector2 50 50) (Vector2 40 190) (Vector2 (350 - (200 * (realToFrac tr))) 100) yellow
  drawTriangleLines (Vector2 50 250) (Vector2 40 390) (Vector2 (150 + (200 * (realToFrac tr))) 300) pink

testDrawPoly :: Double -> IO ()
testDrawPoly tr = do
  drawPoly (Vector2 100 100) (ceiling (20 * tr)) 90 0 white
  drawPolyEx [(Vector2 350 50), (Vector2 250 50), (Vector2 300 300), (Vector2 250 390), (Vector2 350 380)] yellow
  drawPolyExLines [(Vector2 50 210), (Vector2 60 380), (Vector2 150 250)] red



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
