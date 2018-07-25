Ray *WrappedGetMouseRay(Vector2 mousePosition, Camera3D camera);

Vector2 *WrappedGetWorldToScreen(Vector3 position, Camera3D camera);

Matrix *WrappedGetCameraMatrix(Camera3D camera);

Vector4 *WrappedColorNormalize(Color color);

Vector3 *WrappedColorToHSV(Color color);

Color *WrappedGetColor(int hexValue);

Color *WrappedFade(Color color, float alpha);
