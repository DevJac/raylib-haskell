/////////////////////////////////////////////////////////////////////////////////
// Wrapped Unloaders
/////////////////////////////////////////////////////////////////////////////////

void WrappedUnloadImage(Image *image);
void WrappedUnloadTexture(Texture2D *texture);
void WrappedUnloadRenderTexture(RenderTexture2D *texture);
void WrappedUnloadFont(Font *font);
void WrappedUnloadModel(Model *model);
void WrappedUnloadMaterial(Material *material);
void WrappedUnloadShader(Shader *shader);
void WrappedUnloadWave(Wave *wave);
void WrappedUnloadSound(Sound *sound);
void WrappedUnloadMusicStream(Music *music);
void WrappedCloseAudioStream(AudioStream *audioStream);

/////////////////////////////////////////////////////////////////////////////////
// Core
/////////////////////////////////////////////////////////////////////////////////

void WrappedGetMouseRay(Vector2 mousePosition, Camera3D camera, Ray *result);
void WrappedGetWorldToScreen(Vector3 position, Camera3D camera, Vector2 *result);
void WrappedGetCameraMatrix(Camera3D camera, Matrix *result);
void WrappedGetMousePosition(Vector2 *result);
