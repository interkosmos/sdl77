/*
 * SDL77 - SDL 1.2 abstraction layer for FORTRAN 77
 * Copyright (c) 2022, Philipp Engel (ISC Licence)
 *
 * A collection of glue routines that take advantage of FORTRAN/C
 * calling conventions to access SDL, SDL_image, and SDL_mixer from
 * FORTRAN 77.
 *
 * To create the static library libSDL77.a, run:
 *
 * $ gcc `sdl-config --cflags` -c SDL77.c
 * $ ar rcs libSDL77.a SDL77.o
 *
 * Linking a Fortran program against SDL 1.2:
 *
 * $ gfortran `sdl-config --cflags` -o demo demo.f libSDL77.a
 *   `sdl-config --libs` -lSDL_image -lSDL_mixer
 */

#include <time.h>

#include "SDL.h"
#include "SDL_image.h"
#include "SDL_mixer.h"

#define AUDIO_FREQ     MIX_DEFAULT_FREQUENCY    /* 22050 Hz */
#define AUDIO_FORMAT   MIX_DEFAULT_FORMAT       /* AUDIO_S16SYS by default */
#define AUDIO_CHANNELS MIX_DEFAULT_CHANNELS     /* mono or stereo */
#define AUDIO_CHUNK    4096                     /* audio chunk size in bytes */
#define NLAYERS        8                        /* max. number of layer surfaces */
#define SCREEN_BPP     32                       /* colour depth (32 bit) */
#define SDL_FLAG       SDL_SWSURFACE            /* SDL surface flag: use software rendering by default */

#define MIN(a, b) (((a)<(b))?(a):(b))
#define MAX(a, b) (((a)>(b))?(a):(b))

SDL_Event   event;                              /* last event */
SDL_Surface *layers[NLAYERS] = { NULL };        /* layer surfaces */
Mix_Music   *music           = NULL;            /* SDL_mixer handle */

int layer         = 0;                          /* currently selected layer (0 is screen layer) */
int screen_width  = 0;                          /* window width */
int screen_height = 0;                          /* window height */

Uint8  *key_state = NULL;                       /* keyboard state array */
Uint32 *palette   = NULL;                       /* custom palette array */
Uint32 color      = 0;                          /* current pen colour (in screen surface format) */

int  gkey_(int *icode);
long gticks_();
long gtime_();

void galloc_(int *istat);
void gblit_(int *layer, int *ix1, int *iy1, int *ix2, int *iy2, int *iw, int *ih);
void gclose_();
void gcolor_(int *ir, int *ig, int *ib);
void gcppal_(int *ix, int *iy, int *i);
void gcppix_(int *ix1, int *iy1, int *ix2, int *iy2);
void gcreat_(int *iw, int *ih);
void gcur_(int *itoggle);
void gdelay_(int *idelay);
void gevent_(int *ievent, int *istat);
void gfill_();
void gfillr_(int *ix, int *iy, int *iw, int *ih);
void gflush_();
void ggrab_(int *itoggle);
void ghline_(int *ix1, int *ix2, int *iy);
void glayer_(int *layer);
void gload_(const char *file, int *istat);
void glock_();
void gmouse_(long *ixrel, long *iyrel);
void gopen_(int *iw, int *ih, const char *title, int *istat);
void gpal_(int *n);
void gpixel_(int *ix, int *iy);
void gsetp_(int *i, int *ir, int *ig, int *ib);
void gsshot_(const char *file);
void gulock_();
void gvideo_(int *ihw, int *iwm, long long *ivm);
void gvline_(int *ix, int *iy1, int *iy2);
void gwarp_(int *ix, int *iy);

void mclose_();
void mhalt_();
void mopen_(const char *file);
void mplay_(int *loops);

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
long gticks_()
{
    return (long) SDL_GetTicks();
}

/*
 * Returns current time as UNIX timestamp.
 */
long gtime_()
{
    return (long) time(NULL);
}

/*
 * Returns allocation status of given layer. The status is set to 1 if
 * the layer is allocated, else 0.
 */
void galloc_(int *istat)
{
    *istat = 1;
    if (!layers[layer]) return;
    *istat = 0;
}

/*
 * Blits rectangle copied from global layer surface to given layer surface.
 */
void gblit_(int *i, int *ix1, int *iy1, int *ix2, int *iy2, int *iw, int *ih)
{
    SDL_Rect src = { *ix1, *iy1, *iw, *ih };
    SDL_Rect dst = { *ix2, *iy2, *iw, *ih };
    SDL_BlitSurface(layers[layer], &src, layers[*i], &dst);
}

/*
 * Cleans up and quits SDL 1.2.
 */
void gclose_()
{
    for (int i = 1; i < NLAYERS; i++)
    {
        if (layers[i]) SDL_FreeSurface(layers[i]);
    }

    if (palette) free(palette);
    if (music) Mix_FreeMusic(music);

    Mix_CloseAudio();
    Mix_Quit();
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
 * Copies colour from palette to layer surface. The surface has to be
 * locked beforehand.
 */
void gcppal_(int *ix, int *iy, int *i)
{
    Uint32 *pixels = (Uint32 *) layers[layer]->pixels;
    pixels[(*iy * layers[layer]->w) + *ix] = palette[*i];
}

/*
 * Copies pixel from layer surface to screen surface.
 * The screen surface has to be locked beforehand.
 */
void gcppix_(int *ix1, int *iy1, int *ix2, int *iy2)
{
    Uint32 *pixels = (Uint32 *) layers[layer]->pixels;
    Uint32 *scratch = (Uint32 *) layers[0]->pixels;
    scratch[(*iy2 * layers[0]->w) + *ix2] = pixels[(*iy1 * layers[layer]->w) + *ix1];
}

/*
 * Shows or hides the mouse cursor.
 */
void gcur_(int *itoggle)
{
    SDL_ShowCursor(*itoggle);
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
 * Fills screen surface in current colour.
 */
void gfill_()
{
    SDL_FillRect(layers[layer], NULL, color);
}

/*
 * Fills rectangle on screen surface in current colour.
 */
void gfillr_(int *ix, int *iy, int *iw, int *ih)
{
    SDL_Rect rect = { *ix, *iy, *iw, *ih };
    SDL_FillRect(layers[layer], &rect, color);
}

/*
 * Flips screen surface.
 */
void gflush_()
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
 * Loads image from file to current layer, using SDL_image.
 */
void gload_(const char *file, int *istat)
{
    SDL_Surface *image = NULL;

    *istat = -1;
    if (layer <= 0) return;
    image = IMG_Load(file);
    if (!image) return;
    if (layers[layer]) SDL_FreeSurface(layers[layer]);
    layers[layer] = SDL_DisplayFormatAlpha(image);
    SDL_FreeSurface(image);
    if (!layers[layer]) return;
    *istat = 0;
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
 * Locks current layer surface for direct pixel manipulation.
 */
void glock_()
{
    if (SDL_MUSTLOCK(layers[layer])) SDL_LockSurface(layers[layer]);
}

/*
 * Returns relative mouse motion.
 */
void gmouse_(long *ixrel, long *iyrel)
{
    if (event.type == SDL_MOUSEMOTION)
    {
        *ixrel = (long) event.motion.xrel;
        *iyrel = (long) event.motion.yrel;
    }
    else
    {
        *ixrel = 0;
        *iyrel = 0;
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
    if (Mix_Init(MIX_INIT_OGG) == -1) return;

    if (SDL_Init(SDL_INIT_AUDIO | SDL_INIT_VIDEO) == -1) return;
    if (Mix_OpenAudio(AUDIO_FREQ, AUDIO_FORMAT, AUDIO_CHANNELS, AUDIO_CHUNK) == -1) return;

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
void gulock_()
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

/*
 * Closes audio file.
 */
void mclose_()
{
    if (music) Mix_FreeMusic(music);
}

/*
 * Halts audio playback.
 */
void mhalt_()
{
    Mix_HaltMusic();
}

/*
 * Opens audio file.
 */
void mopen_(const char *file)
{
    if (music) Mix_FreeMusic(music);
    music = Mix_LoadMUS(file);
}

/*
 * Plays audio file.
 */
void mplay_(int *loops)
{
    Mix_PlayMusic(music, *loops);
}
