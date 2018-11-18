{-# LANGUAGE ForeignFunctionInterface #-}
module Shapes (

  -- * Basic shapes
  drawPixel,
  drawPixelV,
  drawLine,
  drawLineV,
  drawLineEx,
  drawLineBezier,
  drawCircle,
  drawCircleGradient,
  drawCircleV,
  drawCircleLines,
  drawRectangle,
  drawRectangleV,
  drawRectangleRec,
  drawRectanglePro,
  drawRectangleGradientV,
  drawRectangleGradientH,
  drawRectangleGradientEx,
  drawRectangleLines,
  drawRectangleLinesEx,
  drawTriangle,
  drawTriangleLines,
  drawPoly,
  drawPolyEx,
  drawPolyExLines,

  -- * Collision detection
  -- TODO checkCollisionRecs,
  -- TODO checkCollisionCircles,
  -- TODO checkCollisionCircleRec,
  -- TODO getCollisionRec,
  -- TODO checkCollisionPointRec,
  -- TODO checkCollisionPointCircle,
  -- TODO checkCollisionPointTriangle,

) where
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Types

#include "raylib.h"
#include "shapes.h"

foreign import ccall unsafe "shapes.h WrappedDrawPixel" c_WrappedDrawPixel :: CInt -> CInt -> Ptr Color -> IO ()
drawPixel :: Int -> Int -> Color -> IO ()
drawPixel posX posY color =
  with color $ \colorPtr ->
    c_WrappedDrawPixel (fromIntegral posX) (fromIntegral posY) colorPtr

foreign import ccall unsafe "shapes.h WrappedDrawPixelV" c_WrappedDrawPixelV :: Ptr Vector2 -> Ptr Color -> IO ()
drawPixelV :: Vector2 -> Color -> IO ()
drawPixelV position color =
  with position $ \positionPtr ->
    with color $ \colorPtr ->
      c_WrappedDrawPixelV positionPtr colorPtr

foreign import ccall unsafe "shapes.h WrappedDrawLine" c_WrappedDrawLine :: CInt -> CInt -> CInt -> CInt -> Ptr Color -> IO ()
drawLine :: Int -> Int -> Int -> Int -> Color -> IO ()
drawLine startPosX startPosY endPosX endPosY color =
  with color $ \colorPtr ->
    c_WrappedDrawLine (fromIntegral startPosX) (fromIntegral startPosY) (fromIntegral endPosX) (fromIntegral endPosY) colorPtr

foreign import ccall unsafe "shapes.h WrappedDrawLineV" c_WrappedDrawLineV :: Ptr Vector2 -> Ptr Vector2 -> Ptr Color -> IO ()
drawLineV :: Vector2 -> Vector2 -> Color -> IO ()
drawLineV startPos endPos color =
  with startPos $ \startPosPtr ->
    with endPos $ \endPosPtr ->
      with color $ \colorPtr ->
        c_WrappedDrawLineV startPosPtr endPosPtr colorPtr

foreign import ccall unsafe "shapes.h WrappedDrawLineEx" c_WrappedDrawLineEx :: Ptr Vector2 -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()
drawLineEx :: Vector2 -> Vector2 -> Float -> Color -> IO ()
drawLineEx startPos endPos thick color =
  with startPos $ \startPosPtr ->
    with endPos $ \endPosPtr ->
      with color $ \colorPtr ->
        c_WrappedDrawLineEx startPosPtr endPosPtr (realToFrac thick) colorPtr

foreign import ccall unsafe "shapes.h WrappedDrawLineBezier" c_WrappedDrawLineBezier :: Ptr Vector2 -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()
drawLineBezier :: Vector2 -> Vector2 -> Float -> Color -> IO ()
drawLineBezier startPos endPos thick color =
  with startPos $ \startPosPtr ->
    with endPos $ \endPosPtr ->
      with color $ \colorPtr ->
        c_WrappedDrawLineBezier startPosPtr endPosPtr (realToFrac thick) colorPtr

foreign import ccall unsafe "shapes.h WrappedDrawCircle" c_WrappedDrawCircle :: CInt -> CInt -> CFloat -> Ptr Color -> IO ()
drawCircle :: Int -> Int -> Float -> Color -> IO ()
drawCircle centerX centerY radius color =
  with color $ \colorPtr ->
    c_WrappedDrawCircle (fromIntegral centerX) (fromIntegral centerY) (realToFrac radius) colorPtr

foreign import ccall unsafe "shapes.h WrappedDrawCircleGradient" c_WrappedDrawCircleGradient :: CInt -> CInt -> CFloat -> Ptr Color -> Ptr Color -> IO ()
drawCircleGradient :: Int -> Int -> Float -> Color -> Color -> IO ()
drawCircleGradient centerX centerY radius color1 color2 =
  with color1 $ \color1Ptr ->
    with color2 $ \color2Ptr ->
      c_WrappedDrawCircleGradient (fromIntegral centerX) (fromIntegral centerY) (realToFrac radius) color1Ptr color2Ptr

foreign import ccall unsafe "shapes.h WrappedDrawCircleV" c_WrappedDrawCircleV :: Ptr Vector2 -> CFloat -> Ptr Color -> IO ()
drawCircleV :: Vector2 -> Float -> Color -> IO ()
drawCircleV center radius color =
  with center $ \centerPtr ->
    with color $ \colorPtr ->
      c_WrappedDrawCircleV centerPtr (realToFrac radius) colorPtr

foreign import ccall unsafe "shapes.h WrappedDrawCircleLines" c_WrappedDrawCircleLines :: CInt -> CInt -> CFloat -> Ptr Color -> IO ()
drawCircleLines :: Int -> Int -> Float -> Color -> IO ()
drawCircleLines centerX centerY radius color =
  with color $ \colorPtr ->
    c_WrappedDrawCircleLines (fromIntegral centerX) (fromIntegral centerY) (realToFrac radius) colorPtr

foreign import ccall unsafe "shapes.h WrappedDrawRectangle" c_WrappedDrawRectangle :: CInt -> CInt -> CInt -> CInt -> Ptr Color -> IO ()
drawRectangle :: Int -> Int -> Int -> Int -> Color -> IO ()
drawRectangle posX posY width height color =
  with color $ \colorPtr ->
    c_WrappedDrawRectangle (fromIntegral posX) (fromIntegral posY) (fromIntegral width) (fromIntegral height) colorPtr

foreign import ccall unsafe "shapes.h WrappedDrawRectangleV" c_WrappedDrawRectangleV :: Ptr Vector2 -> Ptr Vector2 -> Ptr Color -> IO ()
drawRectangleV :: Vector2 -> Vector2 -> Color -> IO ()
drawRectangleV position size color =
  with position $ \positionPtr ->
    with size $ \sizePtr ->
      with color $ \colorPtr ->
        c_WrappedDrawRectangleV positionPtr sizePtr colorPtr

foreign import ccall unsafe "shapes.h WrappedDrawRectangleRec" c_WrappedDrawRectangleRec :: Ptr Rectangle -> Ptr Color -> IO ()
drawRectangleRec :: Rectangle -> Color -> IO ()
drawRectangleRec rectangle color =
  with rectangle $ \rectanglePtr ->
    with color $ \colorPtr ->
      c_WrappedDrawRectangleRec rectanglePtr colorPtr

foreign import ccall unsafe "shapes.h WrappedDrawRectanglePro" c_WrappedDrawRectanglePro :: Ptr Rectangle -> Ptr Vector2 -> CFloat -> Ptr Color -> IO ()
drawRectanglePro :: Rectangle -> Vector2 -> Float -> Color -> IO ()
drawRectanglePro rectangle origin rotation color =
  with rectangle $ \rectanglePtr ->
    with origin $ \originPtr ->
      with color $ \colorPtr ->
        c_WrappedDrawRectanglePro rectanglePtr originPtr (realToFrac rotation) colorPtr

foreign import ccall unsafe "shapes.h WrappedDrawRectangleGradientV" c_WrappedDrawRectangleGradientV :: CInt -> CInt -> CInt -> CInt -> Ptr Color -> Ptr Color -> IO ()
drawRectangleGradientV :: Int -> Int -> Int -> Int -> Color -> Color -> IO ()
drawRectangleGradientV posX posY width height color1 color2 =
  with color1 $ \color1Ptr ->
    with color2 $ \color2Ptr ->
      c_WrappedDrawRectangleGradientV (fromIntegral posX) (fromIntegral posY) (fromIntegral width) (fromIntegral height) color1Ptr color2Ptr

foreign import ccall unsafe "shapes.h WrappedDrawRectangleGradientH" c_WrappedDrawRectangleGradientH :: CInt -> CInt -> CInt -> CInt -> Ptr Color -> Ptr Color -> IO ()
drawRectangleGradientH :: Int -> Int -> Int -> Int -> Color -> Color -> IO ()
drawRectangleGradientH posX posY width height color1 color2 =
  with color1 $ \color1Ptr ->
    with color2 $ \color2Ptr ->
      c_WrappedDrawRectangleGradientH (fromIntegral posX) (fromIntegral posY) (fromIntegral width) (fromIntegral height) color1Ptr color2Ptr

foreign import ccall unsafe "shapes.h WrappedDrawRectangleGradientEx" c_WrappedDrawRectangleGradientEx :: Ptr Rectangle -> Ptr Color -> Ptr Color -> Ptr Color -> Ptr Color -> IO ()
drawRectangleGradientEx :: Rectangle -> Color -> Color -> Color -> Color -> IO ()
drawRectangleGradientEx rectangle col1 col2 col3 col4 =
  with rectangle $ \rectanglePtr ->
    with col1 $ \col1Ptr ->
      with col2 $ \col2Ptr ->
        with col3 $ \col3Ptr ->
          with col4 $ \col4Ptr ->
            c_WrappedDrawRectangleGradientEx rectanglePtr col1Ptr col2Ptr col3Ptr col4Ptr

foreign import ccall unsafe "shapes.h WrappedDrawRectangleLines" c_WrappedDrawRectangleLines :: CInt -> CInt -> CInt -> CInt -> Ptr Color -> IO ()
drawRectangleLines :: Int -> Int -> Int -> Int -> Color -> IO ()
drawRectangleLines posX posY width height color =
  with color $ \colorPtr ->
    c_WrappedDrawRectangleLines (fromIntegral posX) (fromIntegral posY) (fromIntegral width) (fromIntegral height) colorPtr

foreign import ccall unsafe "shapes.h WrappedDrawRectangleLinesEx" c_WrappedDrawRectangleLinesEx :: Ptr Rectangle -> CInt -> Ptr Color -> IO ()
drawRectangleLinesEx :: Rectangle -> Int -> Color -> IO ()
drawRectangleLinesEx rectangle lineThick color =
  with rectangle $ \rectanglePtr ->
    with color $ \colorPtr ->
      c_WrappedDrawRectangleLinesEx rectanglePtr (fromIntegral lineThick) colorPtr

foreign import ccall unsafe "shapes.h WrappedDrawTriangle" c_WrappedDrawTriangle :: Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Color -> IO ()
drawTriangle :: Vector2 -> Vector2 -> Vector2 -> Color -> IO ()
drawTriangle v1 v2 v3 color =
  with v1 $ \v1Ptr ->
    with v2 $ \v2Ptr ->
      with v3 $ \v3Ptr ->
        with color $ \colorPtr ->
          c_WrappedDrawTriangle v1Ptr v2Ptr v3Ptr colorPtr

foreign import ccall unsafe "shapes.h WrappedDrawTriangleLines" c_WrappedDrawTriangleLines :: Ptr Vector2 -> Ptr Vector2 -> Ptr Vector2 -> Ptr Color -> IO ()
drawTriangleLines :: Vector2 -> Vector2 -> Vector2 -> Color -> IO ()
drawTriangleLines v1 v2 v3 color =
  with v1 $ \v1Ptr ->
    with v2 $ \v2Ptr ->
      with v3 $ \v3Ptr ->
        with color $ \colorPtr ->
          c_WrappedDrawTriangleLines v1Ptr v2Ptr v3Ptr colorPtr

foreign import ccall unsafe "shapes.h WrappedDrawPoly" c_WrappedDrawPoly :: Ptr Vector2 -> CInt -> CFloat -> CFloat -> Ptr Color -> IO ()
drawPoly :: Vector2 -> Int -> Float -> Float -> Color -> IO ()
drawPoly center sides radius rotation color =
  with center $ \centerPtr ->
    with color $ \colorPtr ->
      c_WrappedDrawPoly centerPtr (fromIntegral sides) (realToFrac radius) (realToFrac rotation) colorPtr

foreign import ccall unsafe "shapes.h WrappedDrawPolyEx" c_WrappedDrawPolyEx :: Ptr Vector2 -> CInt -> Ptr Color -> IO ()
drawPolyEx :: [Vector2] -> Color -> IO ()
drawPolyEx points color =
  withArray points $ \pointsPtr ->
    with color $ \colorPtr ->
      c_WrappedDrawPolyEx pointsPtr (fromIntegral (length points)) colorPtr

foreign import ccall unsafe "shapes.h WrappedDrawPolyExLines" c_WrappedDrawPolyExLines :: Ptr Vector2 -> CInt -> Ptr Color -> IO ()
drawPolyExLines :: [Vector2] -> Color -> IO ()
drawPolyExLines points color =
  withArray points $ \pointsPtr ->
    with color $ \colorPtr ->
      c_WrappedDrawPolyExLines pointsPtr (fromIntegral (length points)) colorPtr
