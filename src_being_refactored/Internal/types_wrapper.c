#include <stdlib.h>
#include "raylib.h"

void WrappedUnloadImage(Image *image) {
    UnloadImage(*image);
    free(image);
}

void WrappedUnloadRenderTexture(RenderTexture2D *target) {
    UnloadRenderTexture(*target);
    free(target);
}
