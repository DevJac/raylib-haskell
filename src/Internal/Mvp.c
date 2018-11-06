#include <stdlib.h>
#include "raylib.h"

// This probably isn't what we want.
// Instead of creating and freeing the pointer in C,
// we should wrap a function and pass in a pointer from Haskell to be populated.
// Except that might not work because we don't know what size to make the pointer within Haskell.
// Do pointers have size?
// What we have here will work, but is it the easiest way?

Font *WrappedGetFontDefault(void) {
  Font *r = malloc(sizeof *r);
  *r = GetFontDefault();
  return r;
}

void WrappedUnloadFont(Font *font) {
  UnloadFont(*font);
  free(font);
}
