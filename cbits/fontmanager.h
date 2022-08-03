// Based on code from memononen's https://github.com/memononen/nanovg

#ifndef FONT_MANAGER_H
#define FONT_MANAGER_H

#include <stdio.h>
#include "fontstash.h"

#define INIT_FONTIMAGE_SIZE 512
#define MAX_FONTIMAGE_SIZE 2048
#define MAX_FONTIMAGES 4
#define ALIGN_LEFT 1
#define ALIGN_BASELINE 64

struct FMcontext {
	struct FONScontext* fs;
	float dpr;
	float scale;
	float fontSize;
	float letterSpacing;
	float lineHeight;
	float fontBlur;
	int textAlign;
	int fontId;
};

typedef struct FMcontext FMcontext;

struct FMGglyphPosition {
	const char* str;  // Position of the glyph in the input string.
	float x;          // The x-coordinate of the logical glyph position.
	float minx, maxx; // The bounds of the glyph shape.
	float miny, maxy; // The vertical bounds of the glyph shape.
};

typedef struct FMGglyphPosition FMGglyphPosition;

FMcontext* fmInit(float dpr);

int fmCreateFont(FMcontext* ctx, const char* name, const char* filename);

int fmCreateFontMem(FMcontext* ctx, const char* name, const char* data, int dataSize);

void fmSetScale(FMcontext* ctx, float scale);

void fmFontFace(FMcontext* ctx, const char* font);

void fmFontSize(FMcontext* ctx, float size);

void fmFontBlur(FMcontext* ctx, float blur);

void fmTextLetterSpacing(FMcontext* ctx, float spacing);

void fmTextLineHeight(FMcontext* ctx, float lineHeight);

void fmTextMetrics(FMcontext* ctx, float* ascender, float* descender, float* lineh);

float fmTextBounds(FMcontext* ctx, float x, float y, const char* string, const char* end, float* bounds);

int fmTextGlyphPositions(FMcontext* ctx, float x, float y, const char* string, const char* end, FMGglyphPosition* positions, int maxPositions);

#endif
