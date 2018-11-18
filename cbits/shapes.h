void WrappedDrawPixel(int posX, int posY, Color *color);
void WrappedDrawPixelV(Vector2 *position, Color *color);
void WrappedDrawLine(int startPosX, int startPosY, int endPosX, int endPosY, Color *color);
void WrappedDrawLineV(Vector2 *startPos, Vector2 *endPos, Color *color);
void WrappedDrawLineEx(Vector2 *startPos, Vector2 *endPos, float thick, Color *color);
void WrappedDrawLineBezier(Vector2 *startPos, Vector2 *endPos, float thick, Color *color);
void WrappedDrawCircle(int centerX, int centerY, float radius, Color *color);
void WrappedDrawCircleGradient(int centerX, int centerY, float radius, Color *color1, Color *color2);
void WrappedDrawCircleV(Vector2 *center, float radius, Color *color);
void WrappedDrawCircleLines(int centerX, int centerY, float radius, Color *color);
void WrappedDrawRectangle(int posX, int posY, int width, int height, Color *color);
void WrappedDrawRectangleV(Vector2 *position, Vector2 *size, Color *color);
void WrappedDrawRectangleRec(Rectangle *rec, Color *color);
void WrappedDrawRectanglePro(Rectangle *rec, Vector2 *origin, float rotation, Color *color);
void WrappedDrawRectangleGradientV(int posX, int posY, int width, int height, Color *color1, Color *color2);
void WrappedDrawRectangleGradientH(int posX, int posY, int width, int height, Color *color1, Color *color2);
void WrappedDrawRectangleGradientEx(Rectangle *rec, Color *col1, Color *col2, Color *col3, Color *col4);
void WrappedDrawRectangleLines(int posX, int posY, int width, int height, Color *color);
void WrappedDrawRectangleLinesEx(Rectangle *rec, int lineThick, Color *color);
void WrappedDrawTriangle(Vector2 *v1, Vector2 *v2, Vector2 *v3, Color *color);
void WrappedDrawTriangleLines(Vector2 *v1, Vector2 *v2, Vector2 *v3, Color *color);
void WrappedDrawPoly(Vector2 *center, int sides, float radius, float rotation, Color *color);
void WrappedDrawPolyEx(Vector2 *points, int numPoints, Color *color);
void WrappedDrawPolyExLines(Vector2 *points, int numPoints, Color *color);
