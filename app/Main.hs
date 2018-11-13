import Control.Concurrent (threadDelay)
import Control.Monad
import Foreign.Storable (Storable(peek))
import Internal.Bindings

main :: IO ()
main = do
  initWindow 500 500 "Hello World"
  loop 0

loop :: Int -> IO ()
loop n = do
  print n
  clearBackground (Color 0 0 0 255)
  beginDrawing
  defaultFont <- getFontDefault
  drawFPS 50 50
  drawTextEx defaultFont "Text is Test" (Vector2 100 100) (fromIntegral n  / 1000) 5 (Color 255 0 0 255)
  mousePos <- getMousePosition
  drawTextEx defaultFont (show mousePos) (Vector2 50 20) 25 5 (Color 255 255 255 255)
  let x = realToFrac $ n `div` 100 `mod` 400
      y = realToFrac $ n `mod` 400
  drawRectangleRec (Rectangle x y (x + 100) (y + 100)) (Color 120 120 255 255)
  endDrawing
  loop (n + 1)
