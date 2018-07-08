{-# LANGUAGE ForeignFunctionInterface #-}
module Lib where
import Foreign.C.Types (CInt(CInt))
import Foreign.C.String (CString, newCString)

foreign import ccall unsafe "raylib.h InitWindow"
    c_initWindow :: CInt -> CInt -> CString -> IO ()

initWindow :: Int -> Int -> String -> IO ()
initWindow width height title = c_initWindow (fromIntegral width) (fromIntegral height) =<< newCString title
