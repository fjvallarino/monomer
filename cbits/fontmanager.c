// Based on code from memononen's https://github.com/memononen/nanovg

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <memory.h>

#include "fontstash.h"
#include "fontmanager.h"

static float fm__minf(float a, float b) {
	return a < b ? a : b;
}

static float fm__maxf(float a, float b) {
	return a > b ? a : b;
}

FMcontext* fmInit(float dpr)
{
	FMcontext *ctx;
  FONSparams fontParams;

	ctx = (FMcontext*) malloc(sizeof(FMcontext));
	memset(ctx, 0, sizeof(FMcontext));

	memset(&fontParams, 0, sizeof(fontParams));
	fontParams.width = INIT_FONTIMAGE_SIZE;
	fontParams.height = INIT_FONTIMAGE_SIZE;
	fontParams.flags = FONS_ZERO_TOPLEFT;
	fontParams.renderCreate = NULL;
	fontParams.renderUpdate = NULL;
	fontParams.renderDraw = NULL;
	fontParams.renderDelete = NULL;
	fontParams.userPtr = NULL;
	
	// Initialize font manager context
	ctx->fs = fonsCreateInternal(&fontParams);
	ctx->dpr = dpr;
	ctx->scale = 1;
	ctx->fontSize = 16.0f;
	ctx->letterSpacing = 0.0f;
	ctx->lineHeight = 1.0f;
	ctx->fontBlur = 0.0f;
	ctx->textAlign = ALIGN_LEFT | ALIGN_BASELINE;
	ctx->fontId = 0;

	return ctx;
}

int fmCreateFont(FMcontext* ctx, const char* name, const char* filename)
{
	return fonsAddFont(ctx->fs, name, filename, 0);
}

int fmCreateFontMem(FMcontext* ctx, const char* name, const char* data, int dataSize)
{
	// Pointer cast, as it was hard to create a Ptr CUChar in Haskell.
	// Could be why it doesn't work.
	const unsigned char* udata = data;
	return fonsAddFontMem(ctx->fs, name, udata, dataSize, 1, 0);
}

void fmSetScale(FMcontext* ctx, float scale) {
	ctx->scale = scale;
}

void fmFontFace(FMcontext* ctx, const char* font)
{
	ctx->fontId = fonsGetFontByName(ctx->fs, font);
}

void fmFontSize(FMcontext* ctx, float size)
{
	ctx->fontSize = size;
}

void fmFontBlur(FMcontext* ctx, float blur)
{
	ctx->fontBlur = blur;
}

void fmTextLetterSpacing(FMcontext* ctx, float spacing)
{
	ctx->letterSpacing = spacing;
}

void fmTextLineHeight(FMcontext* ctx, float lineHeight)
{
	ctx->lineHeight = lineHeight;
}

void fmTextMetrics(FMcontext* ctx, float* ascender, float* descender, float* lineh)
{
	float scale = ctx->dpr * ctx->scale;
	float invscale = 1.0f / scale;

	if (ctx->fontId == FONS_INVALID) return;

	fonsSetSize(ctx->fs, ctx->fontSize * scale);
	fonsSetSpacing(ctx->fs, ctx->letterSpacing * scale);
	fonsSetBlur(ctx->fs, ctx->fontBlur * scale);
	fonsSetAlign(ctx->fs, ctx->textAlign);
	fonsSetFont(ctx->fs, ctx->fontId);

	fonsVertMetrics(ctx->fs, ascender, descender, lineh);
	if (ascender != NULL)
		*ascender *= invscale;
	if (descender != NULL)
		*descender *= invscale;
	if (lineh != NULL)
		*lineh *= invscale;
}

float fmTextBounds(FMcontext* ctx, float x, float y, const char* string, const char* end, float* bounds)
{
	float scale = ctx->dpr * ctx->scale;
	float invscale = 1.0f / scale;
	float width;

	if (ctx->fontId == FONS_INVALID) return 0;

	fonsSetSize(ctx->fs, ctx->fontSize*scale);
	fonsSetSpacing(ctx->fs, ctx->letterSpacing*scale);
	fonsSetBlur(ctx->fs, ctx->fontBlur*scale);
	fonsSetAlign(ctx->fs, ctx->textAlign);
	fonsSetFont(ctx->fs, ctx->fontId);

	width = fonsTextBounds(ctx->fs, x*scale, y*scale, string, end, bounds);
	if (bounds != NULL) {
		// Use line bounds for height.
		fonsLineBounds(ctx->fs, y*scale, &bounds[1], &bounds[3]);
		bounds[0] *= invscale;
		bounds[1] *= invscale;
		bounds[2] *= invscale;
		bounds[3] *= invscale;
	}
	return width * invscale;
}

int fmTextGlyphPositions(FMcontext* ctx, float x, float y, const char* string, const char* end, FMGglyphPosition* positions, int maxPositions)
{
	float scale = ctx->dpr * ctx->scale;
	float invscale = 1.0f / scale;
	FONStextIter iter, prevIter;
	FONSquad q;
	int npos = 0;

	if (ctx->fontId == FONS_INVALID) return 0;

	if (end == NULL)
		end = string + strlen(string);

	if (string == end)
		return 0;

	fonsSetSize(ctx->fs, ctx->fontSize*scale);
	fonsSetSpacing(ctx->fs, ctx->letterSpacing*scale);
	fonsSetBlur(ctx->fs, ctx->fontBlur*scale);
	fonsSetAlign(ctx->fs, ctx->textAlign);
	fonsSetFont(ctx->fs, ctx->fontId);

	fonsTextIterInit(ctx->fs, &iter, x*scale, y*scale, string, end, FONS_GLYPH_BITMAP_OPTIONAL);
	prevIter = iter;

	while (fonsTextIterNext(ctx->fs, &iter, &q)) {
		// can not retrieve glyph?
		if (iter.prevGlyphIndex < 0 && fonsResetAtlas(ctx->fs, MAX_FONTIMAGE_SIZE, MAX_FONTIMAGE_SIZE)) {
			iter = prevIter;
			fonsTextIterNext(ctx->fs, &iter, &q); // try again
		}
		prevIter = iter;
		positions[npos].str = iter.str;
		positions[npos].x = iter.x * invscale;
		positions[npos].minx = fm__minf(iter.x, q.x0) * invscale;
		positions[npos].maxx = fm__maxf(iter.nextx, q.x1) * invscale;
		positions[npos].miny = fm__minf(iter.y, q.y0) * invscale;
		positions[npos].maxy = fm__maxf(iter.nexty, q.y1) * invscale;
		npos++;
		if (npos >= maxPositions)
			break;
	}

	return npos;
}
