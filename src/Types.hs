module Types (
    ConfigFlag
  , KeyboardKey
  , MouseButton
  , LogType
  , CameraType
  , Color
  , Vector2
  , Vector3
  , Vector4
  , Matrix
  , Image
  , RenderTexture2D
  , Camera3D
  , Camera2D
  , Ray
  , GetKeyPressedError(NoKeyPressed, UnknownKeyPressed)
) where
import Internal.Types (
    ConfigFlag
  , KeyboardKey
  , MouseButton
  , LogType
  , CameraType
  , Color
  , Vector2
  , Vector3
  , Vector4
  , Matrix
  , Image
  , RenderTexture2D
  , Camera3D
  , Camera2D
  , Ray
  )

data GetKeyPressedError = NoKeyPressed | UnknownKeyPressed
