#include <stdlib.h>
#include "raylib.h"

Image *WrappedLoadImage(const char *fileName) {
  Image *image = malloc(sizeof *image);
  *image = LoadImage(fileName);
  return image;
}
