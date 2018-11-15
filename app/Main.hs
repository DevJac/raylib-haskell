import Core
import Shapes
import Types

main :: IO ()
main = do
  initWindow 500 500 "Hello World"
  loop 0

loop :: Int -> IO ()
loop n = do
  print n
  clearBackground (Color 0 0 0 255)
  beginDrawing
  let x = realToFrac $ n `div` 100 `mod` 400
      y = realToFrac $ n `mod` 400
  drawRectangleRec (Rectangle x y (x + 100) (y + 100)) (Color 120 120 255 255)
  endDrawing
  loop (n + 1)
