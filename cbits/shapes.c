#include <stdlib.h>
#include "raylib.h"

void WrappedDrawRectangleRec(Rectangle *rec, Color *color) {
  DrawRectangleRec(*rec, *color);
}
