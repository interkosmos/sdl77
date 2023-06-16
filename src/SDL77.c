/*
 * SDL 77 - SDL 1.2 abstraction layer for FORTRAN 77
 * Copyright (c) 2022, Philipp Engel (ISC Licence)
 *
 * A collection of glue routines that take advantage of FORTRAN/C
 * calling conventions to access SDL, SDL_image, and SDL_mixer from
 * FORTRAN 77.
 *
 * To create the static library libSDL77.a, run:
 *
 * $ gcc -fPIC `sdl-config --cflags` -c SDL77.c
 * $ ar rcs libSDL77.a SDL77.o
 *
 * Linking a Fortran program `demo.f` against SDL 1.2:
 *
 * $ gfortran `sdl-config --cflags` -o demo demo.f libSDL77.a
 *   `sdl-config --libs` -lSDL_image -lSDL_mixer
 *
 * Pass NO_IMAGE and/or NO_MIXER to build without external libraries:
 *
 * $ gcc -DNO_IMAGE -DNO_MIXER -fPIC `sdl-config --cflags` -c SDL77.c
 */

#include <time.h>
#include "SDL.h"

#ifndef NO_IMAGE
#include "SDL_image.h"
#endif

#ifndef NO_MIXER
#include "SDL_mixer.h"
#define AUDIO_FREQ     MIX_DEFAULT_FREQUENCY    /* 22050 Hz */
#define AUDIO_FORMAT   MIX_DEFAULT_FORMAT       /* AUDIO_S16SYS by default */
#define AUDIO_CHANNELS MIX_DEFAULT_CHANNELS     /* mono or stereo */
#define AUDIO_CHUNK    4096                     /* audio chunk size in bytes */
#define NCHUNKS        8                        /* max. number of sound effects */
#endif

#define NLAYERS    8                            /* max. number of layer surfaces */
#define SCREEN_BPP 32                           /* colour depth (32 bit) */
#define SDL_FLAG   0                            /* SDL surface flag */

#define MIN(a, b) (((a)<(b))?(a):(b))
#define MAX(a, b) (((a)>(b))?(a):(b))

#ifndef NO_MIXER
Mix_Music *music           = NULL;              /* SDL_mixer handle */
Mix_Chunk *chunks[NCHUNKS] = { NULL };          /* sound effects */
#endif

SDL_Event   event;                              /* last event */
SDL_Surface *layers[NLAYERS] = { NULL };        /* layer surfaces */

int layer         = 0;                          /* currently selected layer (0 is screen layer) */
int screen_width  = 0;                          /* window width */
int screen_height = 0;                          /* window height */

Uint8  *key_state = NULL;                       /* keyboard state array */
Uint32 *palette   = NULL;                       /* custom palette array */
Uint32 color      = 0;                          /* current pen colour (in screen surface format) */

#ifdef __cplusplus
extern "C" {
#endif

int  gkey_(int *icode);
long gticks_(void);
long gtime_(void);

void galloc_(int *);
void gblit_(int *, int *, int *, int *, int *, int *, int *);
void gcirc_(int *, int *, int *);
void gclose_(void);
void gcolor_(int *, int *, int *);
void gcolk_(int *, int *, int *);
void gcppal_(int *, int *, int *);
void gcppix_(int *, int *, int *, int *, int *);
void gcreat_(int *, int *);
void gcur_(int *);
void gdelay_(int *);
void gevent_(int *, int *);
void gfill_(void);
void gfillr_(int *, int *, int *, int *);
void gflush_(void);
void ggrab_(int *);
void ghline_(int *, int *, int *);
void glayer_(int *);
void gline_(int *, int *, int *, int *);
void gload_(const char *, int *);
void glock_(void);
void gmbut_(int *, int *, int *);
void gmouse_(int *, int *, int *, int *);
void gopen_(int *, int *, const char *, int *);
void gpal_(int *);
void gpixel_(int *, int *);
void gsetp_(int *, int *, int *, int *);
void gsshot_(const char *);
void gulock_(void);
void gvideo_(int *, int *, long long *);
void gvline_(int *, int *, int *);
void gwarp_(int *, int *);

#ifndef NO_MIXER
void mchan_(int *, int *, int *);
void mclose_(void);
void mhalt_(void);
void mload_(const char *, int *);
void mloadw_(int *, const char *, int *);
void mpause_(void);
void mplay_(int *);
#endif

/*
 * Returns 1 if key of given code has been pressed, else 0.
 */
int gkey_(int *icode)
{
    return key_state[*icode];
}

/*
 * Returns time in msec since program start.
 */
long gticks_(void)
{
    return (long) SDL_GetTicks();
}

/*
 * Returns current time as UNIX timestamp.
 */
long gtime_(void)
{
    return (long) time(NULL);
}

/*
 * Returns allocation status of given layer. The status is set to 1 if
 * the layer is allocated, else 0.
 */
void galloc_(int *istat)
{
    *istat = -1;
    if (!layers[layer]) return;
    *istat = 0;
}

/*
 * Blits rectangle copied from layer `i` to screen layer.
 */
void gblit_(int *i, int *ix1, int *iy1, int *ix2, int *iy2, int *iw, int *ih)
{
    SDL_Rect src = { (Sint16) *ix1, (Sint16) *iy1, (Uint16) *iw, (Uint16) *ih };
    SDL_Rect dst = { (Sint16) *ix2, (Sint16) *iy2, (Uint16) *iw, (Uint16) *ih };
    SDL_BlitSurface(layers[*i], &src, layers[layer], &dst);
}

/*
 * Draw circle in current colour to selected layer.
 */
void gcirc_(int *ix, int *iy, int *ir)
{
    int d, i, j, sx, sy, x, y;

    x = 0;
    y = *ir - 1;
    d = 3 - (2 * (*ir));
    i = 10 - (4 * (*ir));
    j = 6;

    glock_();

    while (x <= y)
    {
        sx = *ix + x;
        sy = *iy + y;

        gpixel_(&sx, &sy);

        if (d >=  0)
        {
            d += i;
            i += 8;
            y -= 1;
        }
        else
        {
            d += j;
            i += 4;
        }

        j += 4;
        x += 1;
    }

    gulock_();
}

/*
 * Cleans up and quits SDL 1.2.
 */
void gclose_(void)
{
    if (palette) free(palette);

    for (int i = 1; i < NLAYERS; i++)
    {
        if (layers[i]) SDL_FreeSurface(layers[i]);
    }

#ifndef NO_MIXER

    if (music) Mix_FreeMusic(music);

    for (int i = 1; i < NCHUNKS; i++)
    {
        if (chunks[i]) Mix_FreeChunk(chunks[i]);
    }

    Mix_CloseAudio();
    Mix_Quit();

#endif

    SDL_Quit();
}

/*
 * Sets current colour in RGB.
 */
void gcolor_(int *ir, int *ig, int *ib)
{
    color = SDL_MapRGB(layers[0]->format, *ir, *ig, *ib);
}

/*
 * Sets colour key of current layer.
 */
void gcolk_(int *ir, int *ig, int *ib)
{
    SDL_SetColorKey(layers[layer], SDL_SRCCOLORKEY, SDL_MapRGB(layers[layer]->format, *ir, *ig, *ib));
}

/*
 * Copies colour from palette to layer surface. The surface has to be
 * locked beforehand.
 */
void gcppal_(int *i, int *ix, int *iy)
{
    Uint32 *pixels = (Uint32 *) layers[layer]->pixels;
    pixels[(*iy * layers[layer]->w) + *ix] = palette[*i];
}

/*
 * Copies pixel from layer `i` to target layer selected with `glayer_()`.
 * The target layer has to be locked beforehand with `glock_()`.
 */
void gcppix_(int *i, int *ix1, int *iy1, int *ix2, int *iy2)
{
    Uint32 *pixels  = (Uint32 *) layers[*i]->pixels;
    Uint32 *scratch = (Uint32 *) layers[layer]->pixels;
    scratch[(*iy2 * layers[layer]->w) + *ix2] = pixels[(*iy1 * layers[*i]->w) + *ix1];
}

/*
 * Creates a new layer. Replaces existing layer.
 */
void gcreat_(int *iw, int *ih)
{
    if (layers[layer]) SDL_FreeSurface(layers[layer]);
    layers[layer] = SDL_CreateRGBSurface(SDL_FLAG, *iw, *ih, 32, 0, 0, 0, 0);
}

/*
 * Shows or hides the mouse cursor.
 */
void gcur_(int *itoggle)
{
    SDL_ShowCursor(*itoggle);
}

/*
 * Delays program execution for given time in msec.
 */
void gdelay_(int *idelay)
{
    SDL_Delay(*idelay);
}

/*
 * Polls SDL event.
 */
void gevent_(int *ievent, int *istat)
{
    *istat = SDL_PollEvent(&event);
    *ievent = event.type;
}

/*
 * Fills current layer surface in global colour.
 */
void gfill_(void)
{
    SDL_FillRect(layers[layer], NULL, color);
}

/*
 * Fills rectangle on current layer surface in global colour.
 */
void gfillr_(int *ix, int *iy, int *iw, int *ih)
{
    SDL_Rect rect = { (Sint16) *ix, (Sint16) *iy, (Uint16) *iw, (Uint16) *ih };
    SDL_FillRect(layers[layer], &rect, color);
}

/*
 * Flips screen surface.
 */
void gflush_(void)
{
    SDL_Flip(layers[0]);
}

/*
 * Grabs mouse and keyboard input.
 */
void ggrab_(int *itoggle)
{
    if (*itoggle <= 0)
    {
        SDL_WM_GrabInput(SDL_GRAB_OFF);
    }
    else
    {
        SDL_WM_GrabInput(SDL_GRAB_ON);
    }
}

/*
 * Draws horizontal line to layer surface. No bounds checking!
 */
void ghline_(int *ix1, int *ix2, int *iy)
{
    int x1, x2;

    x1 = MIN(*ix1, *ix2);
    x2 = MAX(*ix1, *ix2);

    glock_();

    for (int ix = x1; ix <= x2; ix++)
    {
        gpixel_(&ix, iy);
    }

    gulock_();
}

/*
 * Selects layer.
 */
void glayer_(int *i)
{
    layer = *i;
}

/*
 * Extremely Fast Line Algorithm, Variation A (Division), by Po-Han Lin.
 */
void gline_(int *ix1, int *iy1, int *ix2, int *iy2)
{
    double div;
    int    inc;
    int    llen, slen;
    int    side, swap;
    int    x, y;

    side = 0;
    slen = *iy2 - *iy1;
    llen = *ix2 - *ix1;

    if (abs(slen) > abs(llen))
    {
        swap = slen;
        slen = llen;
        llen = swap;
        side = 1;
    }

    if (llen < 0)
    {
        inc = -1;
    }
    else
    {
        inc = 1;
    }

    if (slen == 0)
    {
        div = llen;
    }
    else
    {
        div = (double) llen / (double) slen;
    }

    glock_();

    if (side)
    {
        for (int i = 0; i != llen; i += inc)
        {
            x = *ix1 + (int) ((double) i / div);
            y = *iy1 + i;

            if (x < 0 || x >= screen_width || y < 0 || y >= screen_height)
                continue;

            gpixel_(&x, &y);
        }
    }
    else
    {
        for (int i = 0; i != llen; i += inc)
        {
            x = *ix1 + i;
            y = *iy1 + (int) ((double) i / div);

            if (x < 0 || x >= screen_width || y < 0 || y >= screen_height)
                continue;

            gpixel_(&x, &y);
        }
    }

    gulock_();
}

/*
 * Loads image from file to current layer, using SDL or SDL_image (if
 * available).
 */
void gload_(const char *file, int *istat)
{
    SDL_Surface *image = NULL;

    *istat = -1;
    if (layer <= 0) return;

#ifdef NO_IMAGE

    image = SDL_LoadBMP(file);
    if (!image) return;
    if (layers[layer]) SDL_FreeSurface(layers[layer]);
    layers[layer] = SDL_DisplayFormat(image);

#else

    image = IMG_Load(file);
    if (!image) return;
    if (layers[layer]) SDL_FreeSurface(layers[layer]);
    layers[layer] = SDL_DisplayFormatAlpha(image);
    SDL_FreeSurface(image);

#endif

    if (!layers[layer]) return;
    *istat = 0;
}

/*
 * Locks current layer surface for direct pixel manipulation.
 */
void glock_(void)
{
    if (SDL_MUSTLOCK(layers[layer])) SDL_LockSurface(layers[layer]);
}

/*
 * Returns mouse button press.
 */
void gmbut_(int *ibut1, int *ibut2, int *ibut3)
{
    *ibut1 = 0; *ibut2 = 0; *ibut3 = 0;

    if (event.type != SDL_MOUSEBUTTONDOWN &&
        event.type != SDL_MOUSEBUTTONUP) return;

    if (event.button.button == SDL_BUTTON_LEFT)   *ibut1 = 1;
    if (event.button.button == SDL_BUTTON_MIDDLE) *ibut2 = 1;
    if (event.button.button == SDL_BUTTON_RIGHT)  *ibut3 = 1;
}

/*
 * Returns mouse motion and absolute mouse position.
 */
void gmouse_(int *ixrel, int *iyrel, int *ix, int *iy)
{
    if (event.type == SDL_MOUSEMOTION)
    {
        *ixrel = (int) event.motion.xrel;
        *iyrel = (int) event.motion.yrel;

        *ix = (int) event.motion.x;
        *iy = (int) event.motion.y;
    }
    else
    {
        *ixrel = 0;
        *iyrel = 0;

        *ix = 0;
        *iy = 0;
    }
}

/*
 * Opens SDL 1.2 window. Only a single window can be opened at a time.
 */
void gopen_(int *iw, int *ih, const char *title, int *istat)
{
    int r, g, b;

    *istat = -1;

    if (SDL_WasInit(SDL_INIT_VIDEO) != 0) return;

#ifndef NO_MIXER
    if (Mix_Init(MIX_INIT_OGG) < 0) return;
#endif

    if (SDL_Init(SDL_INIT_AUDIO | SDL_INIT_VIDEO) < 0) return;

#ifndef NO_MIXER
    if (Mix_OpenAudio(AUDIO_FREQ, AUDIO_FORMAT, AUDIO_CHANNELS, AUDIO_CHUNK) < 0) return;
#endif

    screen_width = *iw;
    screen_height = *ih;
    layers[0] = SDL_SetVideoMode(screen_width, screen_height, SCREEN_BPP, SDL_FLAG);
    if (!layers[0]) return;

    SDL_WM_SetCaption(title, NULL);
    key_state = SDL_GetKeyState(NULL);
    r = 0; g = 0; b = 0;
    gcolor_(&r, &g, &b);

    *istat = 0;
}

/*
 * Allocates palette memory for n colours.
 */
void gpal_(int *n)
{
    if (palette) free(palette);
    palette = (Uint32 *) malloc(*n * sizeof(Uint32));
}

/*
 * Draws single pixel on layer surface. The surface must be locked.
 */
void gpixel_(int *ix, int *iy)
{
    Uint32 *scratch = (Uint32 *) layers[layer]->pixels;
    scratch[(*iy * layers[layer]->w) + *ix] = color;
}

/*
 * Sets palette colour at index i in screen format.
 */
void gsetp_(int *i, int *ir, int *ig, int *ib)
{
    palette[*i] = SDL_MapRGB(layers[0]->format, *ir, *ig, *ib);
}

/*
 * Stores screen surface as BMP file to given path. The path must be
 * null-terminated.
 */
void gsshot_(const char *file)
{
    SDL_SaveBMP(layers[0], file);
}

/*
 * Unlocks layer surface.
 */
void gulock_(void)
{
    if (SDL_MUSTLOCK(layers[layer])) SDL_UnlockSurface(layers[layer]);
}

/*
 * Returns status information (hardware acceleration, window manager,
 * video memory).
 */
void gvideo_(int *ihw, int *iwm, long long *ivm)
{
    const SDL_VideoInfo* info = SDL_GetVideoInfo();

    *ihw = (int) info->hw_available;
    *iwm = (int) info->wm_available;
    *ivm = (long long) info->video_mem;
}

/*
 * Draws vertical line to layer surface. No bounds checking!
 */
void gvline_(int *ix, int *iy1, int *iy2)
{
    int y1, y2;

    y1 = MIN(*iy1, *iy2);
    y2 = MAX(*iy1, *iy2);

    glock_();

    for (int iy = y1; iy <= y2; iy++)
    {
        gpixel_(ix, &iy);
    }

    gulock_();
}

/*
 * Warps mouse.
 */
void gwarp_(int *ix, int *iy)
{
    SDL_WarpMouse((Uint16) *ix, (Uint16) *iy);
}

#ifndef NO_MIXER
/*
 * Plays sound effect `i` in chunks array on channel `ichan`.
 * Set `ichan` to -1 to use the next free channel.
 */
void mchan_(int *i, int *ichan, int *loops)
{
    if (*i < 0 || *i >= NCHUNKS) return;
    if (!chunks[*i]) return;
    Mix_PlayChannel(*ichan, chunks[*i], *loops);
}

/*
 * Closes music file.
 */
void mclose_(void)
{
    if (music) Mix_FreeMusic(music);
}

/*
 * Halts music playback.
 */
void mhalt_(void)
{
    Mix_HaltMusic();
}

/*
 * Loads music file.
 */
void mload_(const char *file, int *istat)
{
    *istat = -1;
    if (music) Mix_FreeMusic(music);
    music = Mix_LoadMUS(file);
    if (music) *istat = 0;
}

/*
 * Loads WAV file to index `i` in chunks array.
 */
void mloadw_(int *i, const char *file, int *istat)
{
    *istat = -1;
    if (*i < 0 || *i >= NCHUNKS) return;
    if (chunks[*i]) Mix_FreeChunk(chunks[*i]);
    chunks[*i] = Mix_LoadWAV(file);
    if (chunks[*i]) *istat = 0;
}

/*
 * Pauses music.
 */
void mpause_(void)
{
    Mix_PauseMusic();
}

/*
 * Plays music.
 */
void mplay_(int *loops)
{
    Mix_PlayMusic(music, *loops);
}
#endif

#ifdef __cplusplus
}
#endif
