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
