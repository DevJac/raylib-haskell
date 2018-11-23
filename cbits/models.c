#include <stdlib.h>
#include "raylib.h"

void WrappedDrawCube(Vector3 *position, float width, float height, float length, Color *color) {
  DrawCube(*position, width, height, length, *color);
}

void WrappedDrawCubeWires(Vector3 *position, float width, float height, float length, Color *color) {
  DrawCubeWires(*position, width, height, length, *color);
}

void WrappedDrawBillboard(Camera3D *camera, Texture2D *texture, Vector3 *center, float size, Color *tint) {
  DrawBillboard(*camera, *texture, *center, size, *tint);
}

Material *WrappedLoadMaterial(const char *fileName) {
  Material *result = malloc(sizeof *result);
  *result = LoadMaterial(fileName);
  return result;
}

Material *WrappedLoadMaterialDefault(void) {
  Material *result = malloc(sizeof *result);
  *result = LoadMaterialDefault();
  return result;
}

void WrappedUnloadMaterial(Material *material) {
  UnloadMaterial(*material);
  free(material);
}
