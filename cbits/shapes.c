#include <stdlib.h>
#include "raylib.h"

void WrappedDrawPixel(int posX, int posY, Color *color) {
  DrawPixel(posX, posY, *color);
}

void WrappedDrawPixelV(Vector2 *position, Color *color) {
  DrawPixelV(*position, *color);
}

void WrappedDrawLine(int startPosX, int startPosY, int endPosX, int endPosY, Color *color) {
  DrawLine(startPosX, startPosY, endPosX, endPosY, *color);
}

void WrappedDrawLineV(Vector2 *startPos, Vector2 *endPos, Color *color) {
  DrawLineV(*startPos, *endPos, *color);
}

void WrappedDrawLineEx(Vector2 *startPos, Vector2 *endPos, float thick, Color *color) {
  DrawLineEx(*startPos, *endPos, thick, *color);
}

void WrappedDrawLineBezier(Vector2 *startPos, Vector2 *endPos, float thick, Color *color) {
  DrawLineBezier(*startPos, *endPos, thick, *color);
}

void WrappedDrawCircle(int centerX, int centerY, float radius, Color *color) {
  DrawCircle(centerX, centerY, radius, *color);
}

void WrappedDrawCircleGradient(int centerX, int centerY, float radius, Color *color1, Color *color2) {
  DrawCircleGradient(centerX, centerY, radius, *color1, *color2);
}

void WrappedDrawCircleV(Vector2 *center, float radius, Color *color) {
  DrawCircleV(*center, radius, *color);
}

void WrappedDrawCircleLines(int centerX, int centerY, float radius, Color *color) {
  DrawCircleLines(centerX, centerY, radius, *color);
}

void WrappedDrawRectangle(int posX, int posY, int width, int height, Color *color) {
  DrawRectangle(posX, posY, width, height, *color);
}

void WrappedDrawRectangleV(Vector2 *position, Vector2 *size, Color *color) {
  DrawRectangleV(*position, *size, *color);
}

void WrappedDrawRectangleRec(Rectangle *rec, Color *color) {
  DrawRectangleRec(*rec, *color);
}

void WrappedDrawRectanglePro(Rectangle *rec, Vector2 *origin, float rotation, Color *color) {
  DrawRectanglePro(*rec, *origin, rotation, *color);
}

void WrappedDrawRectangleGradientV(int posX, int posY, int width, int height, Color *color1, Color *color2) {
  DrawRectangleGradientV(posX, posY, width, height, *color1, *color2);
}

void WrappedDrawRectangleGradientH(int posX, int posY, int width, int height, Color *color1, Color *color2) {
  DrawRectangleGradientH(posX, posY, width, height, *color1, *color2);
}

void WrappedDrawRectangleGradientEx(Rectangle *rec, Color *col1, Color *col2, Color *col3, Color *col4) {
  DrawRectangleGradientEx(*rec, *col1, *col2, *col3, *col4);
}

void WrappedDrawRectangleLines(int posX, int posY, int width, int height, Color *color) {
  DrawRectangleLines(posX, posY, width, height, *color);
}

void WrappedDrawRectangleLinesEx(Rectangle *rec, int lineThick, Color *color) {
  DrawRectangleLinesEx(*rec, lineThick, *color);
}

void WrappedDrawTriangle(Vector2 *v1, Vector2 *v2, Vector2 *v3, Color *color) {
  DrawTriangle(*v1, *v2, *v3, *color);
}

void WrappedDrawTriangleLines(Vector2 *v1, Vector2 *v2, Vector2 *v3, Color *color) {
  DrawTriangleLines(*v1, *v2, *v3, *color);
}

void WrappedDrawPoly(Vector2 *center, int sides, float radius, float rotation, Color *color) {
  DrawPoly(*center, sides, radius, rotation, *color);
}

void WrappedDrawPolyEx(Vector2 *points, int numPoints, Color *color) {
  DrawPolyEx(points, numPoints, *color);
}

void WrappedDrawPolyExLines(Vector2 *points, int numPoints, Color *color) {
  DrawPolyExLines(points, numPoints, *color);
}
