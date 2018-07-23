#include <stdlib.h>
#include "raylib.h"

Ray *WrappedGetMouseRay(Vector2 mousePosition, Camera3D camera) {
    Ray *ray = malloc(sizeof *ray);
    *ray = GetMouseRay(mousePosition, camera);
    return ray;
}

Vector2 *WrappedGetWorldToScreen(Vector3 position, Camera3D camera) {
    Vector2 *vector2 = malloc(sizeof *vector2);
    *vector2 = GetWorldToScreen(position, camera);
    return vector2;
}

Matrix *WrappedGetCameraMatrix(Camera3D camera) {
    Matrix *matrix = malloc(sizeof *matrix);
    *matrix = GetCameraMatrix(camera);
    return matrix;
}
