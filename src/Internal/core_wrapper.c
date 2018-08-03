#include <stdlib.h>
#include "raylib.h"

Ray *WrappedGetMouseRay(Vector2 mousePosition, Camera3D camera) {
    Ray *r = malloc(sizeof *r);
    *r = GetMouseRay(mousePosition, camera);
    return r;
}

Vector2 *WrappedGetWorldToScreen(Vector3 position, Camera3D camera) {
    Vector2 *r = malloc(sizeof *r);
    *r = GetWorldToScreen(position, camera);
    return r;
}

Matrix *WrappedGetCameraMatrix(Camera3D camera) {
    Matrix *r = malloc(sizeof *r);
    *r = GetCameraMatrix(camera);
    return r;
}

Vector4 *WrappedColorNormalize(Color color) {
    Vector4 *r = malloc(sizeof *r);
    *r = ColorNormalize(color);
    return r;
}

Vector3 *WrappedColorToHSV(Color color) {
    Vector3 *r = malloc(sizeof *r);
    *r = ColorToHSV(color);
    return r;
}

Color *WrappedGetColor(int hexValue) {
    Color *r = malloc(sizeof *r);
    *r = GetColor(hexValue);
    return r;
}

Color *WrappedFade(Color color, float alpha) {
    Color *r = malloc(sizeof *r);
    *r = Fade(color, alpha);
    return r;
}

Vector2 *WrappedGetMousePosition(void) {
    Vector2 *r = malloc(sizeof *r);
    *r = GetMousePosition();
    return r;
}

Camera3D *WrappedUpdateCamera(Camera *camera) {
    Camera *r = malloc(sizeof *r);
    *r = *camera;
    UpdateCamera(r);
    return r;
}
