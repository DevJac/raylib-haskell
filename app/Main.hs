import Internal.Types
import Foreign.Storable
import Foreign.Marshal.Utils
import Foreign.Marshal.Alloc

main :: IO ()
main = do
  let d = Camera3D (Vector3 1 2 3) (Vector3 4 5 6) (Vector3 7 8 9) 10 11
  alloca $ \p -> do
    putStrLn $ "Pointer from Haskell main alloca: " ++ (show p)
    poke p d
    print =<< peek p
    print =<< peek p
