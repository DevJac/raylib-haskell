Image *WrappedLoadImage(const char *fileName);
void WrappedUnloadImage(Image *image);
Texture2D *WrappedLoadTexture(const char *fileName);
void WrappedUnloadTexture(Texture2D *texture);
Texture2D *WrappedLoadTextureFromImage(Image *image);
void WrappedDrawTextureEx(Texture2D *texture, Vector2 *position, float rotation, float scale, Color *tint);
