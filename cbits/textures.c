#include <stdlib.h>
#include "raylib.h"

Image *WrappedLoadImage(const char *fileName) {
  Image *result = malloc(sizeof *result);
  *result = LoadImage(fileName);
  return result;
}

void WrappedUnloadImage(Image *image) {
  UnloadImage(*image);
  free(image);
}

Texture2D *WrappedLoadTexture(const char *fileName) {
  Texture2D *result = malloc(sizeof *result);
  *result = LoadTexture(fileName);
  return result;
}

void WrappedUnloadTexture(Texture2D *texture) {
  UnloadTexture(*texture);
  free(texture);
}
