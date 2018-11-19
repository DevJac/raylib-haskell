{-# LANGUAGE ForeignFunctionInterface #-}
module Textures (

  -- * Loading/Unloading functions
  loadImage,
  -- TODO loadImageEx,
  -- TODO loadImagePro,
  -- TODO loadImageRaw,
  -- TODO exportImage,
  -- TODO loadTexture,
  -- TODO loadTextureFromImage,
  -- TODO loadRenderTexture,
  -- TODO unloadTexture,
  -- TODO unloadRenderTexture,
  -- TODO getImageData,
  -- TODO getImageDataNormalized,
  -- TODO getPixelDataSize,
  -- TODO getTextureData,
  -- TODO updateTexture,

  -- * Image manipulation functions
  -- TODO imageCopy,
  -- TODO imageToPOT,
  -- TODO imageFormat,
  -- TODO imageAlphaMask,
  -- TODO imageAlphaClear,
  -- TODO imageAlphaCrop,
  -- TODO imageAlphaPremultiply,
  -- TODO imageCrop,
  -- TODO imageResize,
  -- TODO imageResizeNN,
  -- TODO imageResizeCanvas,
  -- TODO imageMipmaps,
  -- TODO imageDither,
  -- TODO imageText,
  -- TODO imageTextEx,
  -- TODO imageDraw,
  -- TODO imageDrawRectangle,
  -- TODO imageDrawText,
  -- TODO imageDrawTextEx,
  -- TODO imageFlipVertical,
  -- TODO imageFlipHorizontal,
  -- TODO imageRotateCW,
  -- TODO imageRotateCCW,
  -- TODO imageColorTint,
  -- TODO imageColorInvert,
  -- TODO imageColorGrayscale,
  -- TODO imageColorContrast,
  -- TODO imageColorBrightness,
  -- TODO imageColorReplace,

  -- * Image generation functions
  -- TODO genImageColor,
  -- TODO genImageGradientV,
  -- TODO genImageGradientH,
  -- TODO genImageGradientRadial,
  -- TODO getImageChecked,
  -- TODO getImageWhiteNoise,
  -- TODO getImagePerlinNoise,
  -- TODO getImageCellular,

  -- * Texture2D configuration functions
  -- TODO getTextureMipmaps,
  -- TODO setTextureFilter,
  -- TODO setTextureWrap,

  -- * Texture2D drawing functions
  -- TODO drawTexture,
  -- TODO drawTextureV,
  -- TODO drawTextureEx,
  -- TODO drawTextureRec,
  -- TODO drawTexturePro,

) where
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Ptr
import Types

#include "raylib.h"
#include "textures.h"

foreign import ccall unsafe "textures.h WrappedLoadImage" c_WrappedLoadImage :: CString -> IO (Ptr Image)
loadImage :: String -> IO Image
loadImage filename =
  withCString filename $ \cFilename -> do
    imagePtr <- c_WrappedLoadImage cFilename
    Image <$> newForeignPtr c_WrappedUnloadImage imagePtr

foreign import ccall unsafe "textures.h &WrappedUnloadImage" c_WrappedUnloadImage :: FunPtr (Ptr Image -> IO ())
