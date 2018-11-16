#include <stdlib.h>
#include "raylib.h"

void CTest(void) {
}

void WrappedClearBackground(Color *color) {
  ClearBackground(*color);
}

void WrappedSetWindowIcon(Image *image) {
  SetWindowIcon(*image);
}
