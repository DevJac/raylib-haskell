#include <stdlib.h>
#include "raylib.h"

void WrappedDrawCube(Vector3 *position, float width, float height, float length, Color *color) {
  DrawCube(*position, width, height, length, *color);
}

void WrappedDrawCubeWires(Vector3 *position, float width, float height, float length, Color *color) {
  DrawCubeWires(*position, width, height, length, *color);
}
