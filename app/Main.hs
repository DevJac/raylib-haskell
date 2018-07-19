import Control.Concurrent
import Lib
import System.Mem

main :: IO ()
main = do
    threadDelay (6 * 1000 * 1000)
    main2
    performMajorGC
    threadDelay (6 * 1000 * 1000)

main2 :: IO ()
main2 = do
    initWindow 200 200 "Hello World"
    image_temp <- loadImage "raylib_16x16.png"
    image <- loadImage "raylib_16x16.png"
    setWindowIcon image
    threadDelay (6 * 1000 * 1000)
