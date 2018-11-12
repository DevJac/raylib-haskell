#include <stdlib.h>
#include "raylib.h"

/////////////////////////////////////////////////////////////////////////////////
// Wrapped Unloaders
/////////////////////////////////////////////////////////////////////////////////

void WrappedUnloadImage(Image *image) {
  UnloadImage(*image);
  free(image);
}

void WrappedUnloadTexture(Texture2D *texture) {
  UnloadTexture(*texture);
  free(texture);
}

void WrappedUnloadRenderTexture(RenderTexture2D *texture) {
  UnloadRenderTexture(*texture);
  free(texture);
}

void WrappedUnloadFont(Font *font) {
  UnloadFont(*font);
  free(font);
}

void WrappedUnloadModel(Model *model) {
  UnloadModel(*model);
  free(model);
}

void WrappedUnloadMaterial(Material *material) {
  UnloadMaterial(*material);
  free(material);
}

void WrappedUnloadShader(Shader *shader) {
  UnloadShader(*shader);
  free(shader);
}

void WrappedUnloadWave(Wave *wave) {
  UnloadWave(*wave);
  free(wave);
}

void WrappedUnloadSound(Sound *sound) {
  UnloadSound(*sound);
  free(sound);
}

void WrappedUnloadMusicStream(Music *music) {
  UnloadMusicStream(*music);
  free(music);
}

void WrappedCloseAudioStream(AudioStream *audioStream) {
  CloseAudioStream(*audioStream);
  free(audioStream);
}
