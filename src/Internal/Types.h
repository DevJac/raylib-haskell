typedef struct WrCamera3D {
  Vector3 *position;
  Vector3 *target;
  Vector3 *up;
  float fovy;
  int type;
} WrCamera3D;

void WrapCamera3D(Camera3D RawCam, WrCamera3D *WrCam);

void UnwrapToCamera3D(WrCamera3D WrCam, Camera3D *RawCam);
