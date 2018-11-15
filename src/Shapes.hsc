{-# LANGUAGE ForeignFunctionInterface #-}
module Shapes (

  -- * Basic shapes
  -- TODO drawPixel,
  -- TODO drawPixelV,
  -- TODO drawLine,
  -- TODO drawLineV,
  -- TODO drawLineEx,
  -- TODO drawLineBezier,
  -- TODO drawCircle,
  -- TODO drawCircleGradient,
  -- TODO drawCircleV,
  -- TODO drawCircleLines,
  -- TODO drawRectangle,
  -- TODO drawRectangleV,
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
import Foreign.Marshal.Utils
import Foreign.Ptr
import Types

#include "raylib.h"
#include "shapes.h"

foreign import ccall unsafe "shapes.h WrappedDrawRectangleRec" c_WrappedDrawRectangleRec :: Ptr Rectangle -> Ptr Color -> IO ()
drawRectangleRec :: Rectangle -> Color -> IO ()
drawRectangleRec rectangle color =
  with rectangle $ \rectanglePtr ->
    with color $ \colorPtr ->
      c_WrappedDrawRectangleRec rectanglePtr colorPtr
