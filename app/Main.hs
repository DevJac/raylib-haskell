import Control.Concurrent
import Core
import Shapes
import Types

main :: IO ()
main = do
  setTraceLog [Info, Warning, Error, Other]
  initWindow 500 500 "Hello World"
  threadDelay (3 * 1000 * 1000)
  loop 0

loop :: Int -> IO ()
loop n = do
  traceLog Info "My trace log!"
  print n
  clearBackground (Color 0 0 0 255)
  beginDrawing
  let x = realToFrac $ n `div` 100 `mod` 400
      y = realToFrac $ n `mod` 400
  drawRectangleRec (Rectangle x y (x + 100) (y + 100)) (Color 120 120 255 255)
  endDrawing
  loop (n + 1)
