import Control.Concurrent (threadDelay)
import Control.Monad
import System.Random
import Mvp

main :: IO ()
main = do
  initWindow 500 500 "Hello World"
  threadDelay (3 * 1000 * 1000)
  loop 0

loop n = do
  print n
  clearBackground (Color 0 0 0 255)
  beginDrawing
  x <- randomRIO (0, 400)
  y <- randomRIO (0, 400)
  drawRectangleRec (Rectangle x y (x + 100) (y + 100)) (Color 120 120 255 255)
  endDrawing
  loop (n + 1)
