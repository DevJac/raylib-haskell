Font *WrappedGetFontDefault(void);
Font *WrappedLoadFont(const char *fileName);
void WrappedDrawTextEx(Font *font, const char *text, Vector2 *position, float fontSize, float spacing, Color *color);
void WrappedUnloadFont(Font *font);
void WrappedDrawText(const char *text, int posX, int posY, int fontSize, Color *color);
void WrappedMeasureTextEx(Font *font, const char *text, float fontSize, float spacing, Vector2 *result);
