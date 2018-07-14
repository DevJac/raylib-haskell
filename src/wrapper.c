#include "raylib.h"

Image *WrappedLoadImage(const char *fileName) {
  Image *image;
  *image = LoadImage(fileName);
  return image;
}
