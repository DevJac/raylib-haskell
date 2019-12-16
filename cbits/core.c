#include <stdlib.h>
#include "raylib.h"

void WrappedClearBackground(Color *color) {
  ClearBackground(*color);
}

void WrappedSetWindowIcon(Image *image) {

  /*
    There is a memory leak here.
    This is done on purpose because there is a double free error if you
    unload the image (using UnloadImage) given to SetWindowIcon.
    See: https://github.com/raysan5/raylib/issues/689
    I am working around this issue by giving SetWindowIcon a copy of the image.
  */
  Image imageCopy = ImageCopy(*image);

  SetWindowIcon(imageCopy);
}

void WrappedGetMousePosition(Vector2 *result) {
  *result = GetMousePosition();
}

void WrappedBeginMode3D(Camera3D *camera) {
  BeginMode3D(*camera);
}

void WrappedSetCameraMode(Camera3D *camera, int mode) {
  SetCameraMode(*camera, mode);
}
