{-# LANGUAGE ForeignFunctionInterface #-}
module Text (

  -- * Font loading/unloading functions
  getFontDefault,
  loadFont,
  -- TODO loadFontEx,
  -- TODO loadFontData,
  -- TODO genImageFontAtlas,

  -- * Text drawing functions
  drawFPS,
  drawText,
  drawTextEx,

  -- * Text misc. functions
  measureText,
  measureTextEx,
  -- TODO getGlyphIndex,

) where
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Types

#include "raylib.h"
#include "text.h"

foreign import ccall unsafe "text.h WrappedGetFontDefault" c_WrappedGetFontDefault :: IO (Ptr Font)
getFontDefault :: IO Font
getFontDefault = do
  fontPtr <- c_WrappedGetFontDefault
  Font <$> newForeignPtr c_WrappedUnloadFont fontPtr

foreign import ccall unsafe "text.h WrappedLoadFont" c_WrappedLoadFont :: CString -> IO (Ptr Font)
loadFont :: String -> IO Font
loadFont fileName =
  withCString fileName $ \cFileName -> do
    fontPtr <- c_WrappedLoadFont cFileName
    Font <$> newForeignPtr c_WrappedUnloadFont fontPtr

foreign import ccall unsafe "text.h &WrappedUnloadFont" c_WrappedUnloadFont :: FunPtr (Ptr Font -> IO ())

foreign import ccall unsafe "raylib.h DrawFPS" c_DrawFPS :: CInt -> CInt -> IO ()
drawFPS :: Int -> Int -> IO ()
drawFPS posX posY = c_DrawFPS (fromIntegral posX) (fromIntegral posY)

foreign import ccall unsafe "text.h WrappedDrawText" c_WrappedDrawText :: CString -> CInt -> CInt -> CInt -> Ptr Color -> IO ()
drawText :: String -> Int -> Int -> Int -> Color -> IO ()
drawText text posX posY fontSize color =
  withCString text $ \textPtr ->
    with color $ \colorPtr ->
      c_WrappedDrawText textPtr (fromIntegral posX) (fromIntegral posY) (fromIntegral fontSize) colorPtr

foreign import ccall unsafe "text.h WrappedDrawTextEx" c_WrappedDrawTextEx :: Ptr Font -> CString -> Ptr Vector2 -> CFloat -> CFloat -> Ptr Color -> IO ()
drawTextEx :: Font -> String -> Vector2 -> Float -> Float -> Color -> IO ()
drawTextEx (Font fontForeignPtr) text position fontSize spacing color =
  withForeignPtr fontForeignPtr $ \fontPtr ->
    withCString text $ \textPtr ->
      with position $ \positionPtr ->
        with color $ \colorPtr ->
          c_WrappedDrawTextEx fontPtr textPtr positionPtr (realToFrac fontSize) (realToFrac spacing) colorPtr

foreign import ccall unsafe "raylib.h MeasureText" c_MeasureText :: CString -> CInt -> IO CInt
measureText :: String -> Int -> IO Int
measureText text fontSize =
  withCString text $ \cText ->
    fromIntegral <$> c_MeasureText cText (fromIntegral fontSize)

foreign import ccall unsafe "text.h WrappedMeasureTextEx" c_WrappedMeasureTextEx :: Ptr Font -> CString -> CFloat -> CFloat -> Ptr Vector2 -> IO ()
measureTextEx :: Font -> String -> Float -> Float -> IO Vector2
measureTextEx (Font fontForeignPtr) text fontSize spacing =
  alloca $ \vector2ResultPtr ->
    withForeignPtr fontForeignPtr $ \fontPtr ->
      withCString text $ \cText -> do
        c_WrappedMeasureTextEx fontPtr cText (realToFrac fontSize) (realToFrac spacing) vector2ResultPtr
        peek vector2ResultPtr
