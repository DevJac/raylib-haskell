#include <stdlib.h>
#include "raylib.h"

void MaterialSetMap(Material *material, int mapType, MaterialMap *map) {
  material->maps[mapType] = *map;
}

void ModelSetMesh(Model *model, Mesh *mesh) {
  model->mesh = *mesh;
}

void ModelSetMaterial(Model *model, Material *material) {
  model->material = *material;
}
