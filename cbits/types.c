#include <stdlib.h>
#include "raylib.h"

void MaterialSetMap(Material *material, int mapType, MaterialMap *map) {
  material->maps[mapType] = *map;
}

void ModelSetMeshes(Model *model, Mesh *meshes, int meshCount) {
  model->meshCount = meshCount;
  model->meshes = meshes;
}

void ModelSetMaterials(Model *model, Material *materials, int materialCount) {
  model->materialCount = materialCount;
  model->materials = materials;
}
