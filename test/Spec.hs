import Control.Concurrent (threadDelay)
import Lib (initWindow)

main :: IO ()
main = do
    initWindow 200 200 "Hello World"
    threadDelay (2 * 1000 * 1000)
