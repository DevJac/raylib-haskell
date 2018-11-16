#include <stdlib.h>
#include "raylib.h"

Font *WrappedGetFontDefault(void) {
  Font *result = malloc(sizeof *result);
  *result = GetFontDefault();
  return result;
}

void WrappedDrawTextEx(Font *font, const char *text, Vector2 *position, float fontSize, float spacing, Color *color) {
  DrawTextEx(*font, text, *position, fontSize, spacing, *color);
}

void WrappedUnloadFont(Font *font) {
  UnloadFont(*font);
  free(font);
}

void WrappedDrawText(const char *text, int posX, int posY, int fontSize, Color *color) {
  DrawText(text, posX, posY, fontSize, *color);
}
