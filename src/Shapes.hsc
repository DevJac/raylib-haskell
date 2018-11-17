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
  -- TODO drawRectanglePro,
  -- TODO drawRectangleGradientV,
  -- TODO drawRectangleGradientH,
  -- TODO drawRectangleGradientEx,
  -- TODO drawRectangleLines,
  -- TODO drawRectangleLinesEx,
  -- TODO drawTriangle,
  -- TODO drawTriangleLines,
  -- TODO drawPoly,
  -- TODO drawPolyEx,
  -- TODO drawPolyExLines,

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
