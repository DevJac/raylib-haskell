#include <stdlib.h>
#include "raylib.h"

/////////////////////////////////////////
// Image wrappers
/////////////////////////////////////////

Image *WrappedLoadImage(const char *fileName) {
    Image *image = malloc(sizeof *image);
    *image = LoadImage(fileName);
    return image;
}

void WrappedUnloadImage(Image *image) {
    UnloadImage(*image);
    free(image);
}
