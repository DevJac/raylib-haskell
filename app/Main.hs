import Control.Concurrent
import Lib
import System.Mem
import Foreign.ForeignPtr

main :: IO ()
main = do
    threadDelay (6 * 1000 * 1000)
    main2
    performMajorGC
    threadDelay (6 * 1000 * 1000)

main2 :: IO ()
main2 = do
    initWindow 200 200 "Hello World"
    image_temp <- loadImage "test_data/raylib_16x16.png"
    w <- imageWidth image_temp
    print w
    image <- loadImage "test_data/raylib_16x16.png"
    setWindowIcon image
    threadDelay (6 * 1000 * 1000)
