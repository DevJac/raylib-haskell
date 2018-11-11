#include <stdlib.h>
#include "raylib.h"

typedef struct WrCamera3D {
  Vector3 *position;
  Vector3 *target;
  Vector3 *up;
  float fovy;
  int type;
} WrCamera3D;

void WrapCamera3D(Camera3D RawCam, WrCamera3D *WrCam) {
  WrCam->position = &RawCam.position;
  WrCam->target   = &RawCam.target;
  WrCam->up       = &RawCam.up;
  WrCam->fovy     =  RawCam.fovy;
  WrCam->type     =  RawCam.type;
}

void UnwrapToCamera3D(WrCamera3D WrCam, Camera3D *RawCam) {
  RawCam->position = *WrCam.position;
  RawCam->target   = *WrCam.target;
  RawCam->up       = *WrCam.up;
  RawCam->fovy     =  WrCam.fovy;
  RawCam->type     =  WrCam.type;
}
