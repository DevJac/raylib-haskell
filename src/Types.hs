module Types (
  -- * Enum Types
    ConfigFlag
  , LogType
  , CameraType
  , KeyboardKey
  , MouseButton
  , Gamepad
  , GamepadButton
  , GamepadAxis
  , Gesture
  -- * Other Types
  , GetPressedError (NothingPressed, UnknownPressed)
  -- * Simple Types
  , Color
  , Rectangle
  , Vector2
  , Vector3
  , Vector4
  , Quaternion
  , Matrix
  , Camera3D
  , Camera2D
  , Ray
  , RayHitInfo
  -- * Complex Types
  , Image
  , Texture2D
  , RenderTexture2D
  , Font
  , Model
  , Mesh
  , Material
  , Shader
  , Wave
  , Sound
  , Music
  , AudioStream
  ) where
import Internal.Bindings (
    ConfigFlag
  , KeyboardKey
  , MouseButton
  , LogType
  , CameraType
  , Gamepad
  , GamepadButton
  , GamepadAxis
  , Gesture
  , Color
  , Rectangle
  , Vector2
  , Vector3
  , Vector4
  , Quaternion
  , Matrix
  , Camera3D
  , Camera2D
  , Ray
  , RayHitInfo
  , Image
  , Texture2D
  , RenderTexture2D
  , Font
  , Model
  , Mesh
  , Material
  , Shader
  , Wave
  , Sound
  , Music
  , AudioStream
  )

data GetPressedError = NothingPressed | UnknownPressed
