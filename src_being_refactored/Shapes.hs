module Shapes where
import qualified Internal.Shapes
import           Types (
    Color
  , Rectangle
  , Vector2
  )

drawPixel :: Int -> Int -> Color -> IO ()
drawPixel posX posY color = Internal.Shapes.drawPixel posX posY color

drawPixelV :: Vector2 -> Color -> IO ()
drawPixelV position color = Internal.Shapes.drawPixelV position color

drawLine :: Int -> Int -> Int -> Int -> Color -> IO ()
drawLine startPosX startPosY endPosX endPosY color = Internal.Shapes.drawLine startPosX startPosY endPosX endPosY color

drawLineV :: Vector2 -> Vector2 -> Color -> IO ()
drawLineV startPos endPos color = Internal.Shapes.drawLineV startPos endPos color

drawLineEx :: Vector2 -> Vector2 -> Float -> Color -> IO ()
drawLineEx startPos endPos thick color = Internal.Shapes.drawLineEx startPos endPos thick color

drawLineBezier :: Vector2 -> Vector2 -> Float -> Color -> IO ()
drawLineBezier startPos endPos thick color = Internal.Shapes.drawLineBezier startPos endPos thick color

drawCircle :: Int -> Int -> Float -> Color -> IO ()
drawCircle centerX centerY radius color = Internal.Shapes.drawCircle centerX centerY radius color

drawCircleGradient :: Int -> Int -> Float -> Color -> Color -> IO ()
drawCircleGradient centerX centerY radius color1 color2 = Internal.Shapes.drawCircleGradient centerX centerY radius color1 color2

drawCircleV :: Vector2 -> Float -> Color -> IO ()
drawCircleV center radius color = Internal.Shapes.drawCircleV center radius color

drawCircleLines :: Int -> Int -> Float -> Color -> IO ()
drawCircleLines centerX centerY radius color = Internal.Shapes.drawCircleLines centerX centerY radius color

drawRectangle :: Int -> Int -> Int -> Int -> Color -> IO ()
drawRectangle posX posY width height color = Internal.Shapes.drawRectangle posX posY width height color

drawRectangleV :: Vector2 -> Vector2 -> Color -> IO ()
drawRectangleV position size color = Internal.Shapes.drawRectangleV position size color

drawRectangleRec :: Rectangle -> Color -> IO ()
drawRectangleRec rec color = Internal.Shapes.drawRectangleRec rec color

drawRectanglePro :: Rectangle -> Vector2 -> Float -> Color -> IO ()
drawRectanglePro rec origin rotation color = Internal.Shapes.drawRectanglePro rec origin rotation color

drawRectangleGradientV :: Int -> Int -> Int -> Int -> Color -> Color -> IO ()
drawRectangleGradientV posX posY width height color1 color2 = Internal.Shapes.drawRectangleGradientV posX posY width height color1 color2

drawRectangleGradientH :: Int -> Int -> Int -> Int -> Color -> Color -> IO ()
drawRectangleGradientH posX posY width height color1 color2 = Internal.Shapes.drawRectangleGradientH posX posY width height color1 color2

drawRectangleGradientEx :: Rectangle -> Color -> Color -> Color -> Color -> IO ()
drawRectangleGradientEx rec col1 col2 col3 col4 = Internal.Shapes.drawRectangleGradientEx rec col1 col2 col3 col4

drawRectangleLines :: Int -> Int -> Int -> Int -> Color -> IO ()
drawRectangleLines posX posY width height color = Internal.Shapes.drawRectangleLines posX posY width height color

drawRectangleLinesEx :: Rectangle -> Int -> Color -> IO ()
drawRectangleLinesEx rec lineThick color = Internal.Shapes.drawRectangleLinesEx rec lineThick color

drawTriangle :: Vector2 -> Vector2 -> Vector2 -> Color -> IO ()
drawTriangle v1 v2 v3 color = Internal.Shapes.drawTriangle v1 v2 v3 color

drawTriangleLines :: Vector2 -> Vector2 -> Vector2 -> Color -> IO ()
drawTriangleLines v1 v2 v3 color = Internal.Shapes.drawTriangleLines v1 v2 v3 color

drawPoly :: Vector2 -> Int -> Float -> Float -> Color -> IO ()
drawPoly center sides radius rotation color = Internal.Shapes.drawPoly center sides radius rotation color

drawPolyEx :: [Vector2] -> Color -> IO ()
drawPolyEx = _todo

drawPolyExLines :: [Vector2] -> Color -> IO ()
drawPolyExLines = _todo
