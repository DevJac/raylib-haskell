{-# LANGUAGE ForeignFunctionInterface #-}
module Internal (cTest) where

foreign import ccall "internal.h cTest" c_cTest :: IO ()
cTest :: IO ()
cTest = c_cTest
