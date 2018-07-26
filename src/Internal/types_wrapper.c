#include <stdlib.h>
#include "raylib.h"

void WrappedUnloadImage(Image *image) {
    UnloadImage(*image);
    free(image);
}

void WrappedUnloadRenderTexture(RenderTexture *target) {
    UnloadRenderTexture(*target);
    free(target);
}
