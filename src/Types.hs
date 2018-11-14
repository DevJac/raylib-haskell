module Types (
  -- * Simple Types
    Color
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
  -- * Enum Types
  , KeyboardKey
  , MouseButton
  , CameraType
  , Gamepad
  , GamepadButton
  , GamepadAxis
  , Gesture
  , ConfigFlag
  , LogType
  -- * Other Types
  , GetPressedError (NothingPressed, UnknownPressed)
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
  , Gestures
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

type Gesture = Gestures

data GetPressedError = NothingPressed | UnknownPressed
