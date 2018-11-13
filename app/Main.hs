import Internal.Bindings
import Foreign.Storable
import Foreign.Marshal.Utils
import Foreign.Marshal.Alloc
import Control.Concurrent

main :: IO ()
main = do
  initWindow 500 500 "Test"
  threadDelay (4 * 1000 * 1000)
  icon <- loadImage "test_data/raylib_16x16.png"
  setWindowIcon icon
  threadDelay (4 * 1000 * 1000)
  closeWindow
