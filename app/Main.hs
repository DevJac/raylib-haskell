import Control.Concurrent (threadDelay)
import Control.Monad
import System.Random
import Mvp

main :: IO ()
main = do
  initWindow 500 500 "Hello World"
  threadDelay (3 * 1000 * 1000)
  loop 0

loop :: Int -> IO ()
loop n = do
  print n
  clearBackground (Color 0 0 0 255)
  beginDrawing
  defaultFont <- getFontDefault
  bs <- fontBaseSize defaultFont
  drawFPS 50 50
  drawTextEx defaultFont (show bs) (Vector2 200 50) 50 5 (Color 255 255 0 255)
  drawTextEx defaultFont "Text is Test" (Vector2 100 100) (fromIntegral n  / 1000) 5 (Color 255 0 0 255)
  x <- randomRIO (0, 400)
  y <- randomRIO (0, 400)
  drawRectangleRec (Rectangle x y (x + 100) (y + 100)) (Color 120 120 255 255)
  endDrawing
  loop (n + 1)
