import Control.Concurrent (threadDelay)
import Mvp (initWindow, closeWindow)

main :: IO ()
main = do
    initWindow 500 500 "Hello World"
    threadDelay (6 * 1000 * 1000)
    closeWindow
