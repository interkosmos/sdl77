# SDL 77 API Documentation

## Functions

### INTEGER FUNCTION GKEY(ICODE)
Returns `1` if key of given key code has been pressed, else `0`.

### INTEGER\*8 FUNCTION GTICKS()
Returns number of milliseconds since the SDL library initialisation as 64-bit
integer.

### INTEGER\*8 FUNCTION GTIME()
Returns UNIX timestamp as 64-bit integer.

## Subroutines

### SUBROUTINE GALLOC(ISTAT)
Returns allocation status of given layer. The status is `1` if the layer is
allocated, else `0`.

### SUBROUTINE GBLIT(I, IX1, IY1, IX2, IY2, IW, IH)
Blits rectangle copied from source layer `I` to target layer selected with
`GLAYER()`.

### SUBROUTINE GMBUT(IBUT1, IBUT2, IBUT3)
Returns mouse button clicks (left, middle, right). On click, the button state
is set to `1`.

### SUBROUTINE GMOUSE(IXREL, IXREL, IX, IY)
Returns relative mouse motion and absolute mouse position.

### SUBROUTINE GCLOSE()
Cleans up and quits SDL 1.2.

### SUBROUTINE GCOLOR(IR, IG, IB)
Sets current colour in RGB.

### SUBROUTINE GCOLK(IR, IG, IB)
Sets colour key (transparent colour) in current layer. Not necessary for images
with alpha channels that have been loaded with SDL_image.

### SUBROUTINE GCPPAL(I, IX, IY)
Copies palette colour `I` to current layer surface. The surface has to be
locked beforehand with `GLOCK()`.

### SUBROUTINE GCPPIX(I, IX1, IY1, IX2, IY2)
Copies pixel from given layer `I` to target layer selected with `GLAYER()`. The
target layer has to be locked beforehand with `GLOCK()`.

### SUBROUTINE GCREAT(IW, IH)
Creates a new layer selected with `GLAYER()` of given size. Replaces existing
layer. The default background colour is black.

### SUBROUTINE GCUR(ITOGGLE)
Shows or hides the mouse cursor, depending on `ITOGGLE` (`0` or `1`).

### SUBROUTINE GDELAY(IDELAY)
Delays program execution for given time in msec.

### SUBROUTINE GEVENT(IEVENT, ISTAT)
Polls an SDL event. The event type is returned in `IEVENT`, the status in
`ISTAT`. If `ISTAT` is `1`, another event is waiting.

### SUBROUTINE GFILL()
Fills current layer in global colour.

### SUBROUTINE GFILLR(IX, IY, IW, IH)
Fills rectangle of given size in global colour.

### SUBROUTINE GFLUSH()
Flips screen surface.

### SUBROUTINE GGRAB(ITOGGLE)
Grabs mouse and keyboard input if `ITOGGLE` is `1`.

### SUBROUTINE GHLINE(IX1, IX2, IY)
Draws horizontal line to layer surface. No bounds checking!

### SUBROUTINE GLAYER(LAYER)
Selects layer by index. The screen layer is alway `0`. Up to 7 additional layers
can be created, either as buffer or to store images loaded from file.

### SUBROUTINE GLINE(IX1, IY1, IX2, IY2)
Draws a line from `IX1`, `IY1` to `IX2`, `IY2`.

### SUBROUTINE GLOAD(FILE, ISTAT)
Loads image from file to current layer, using SDL or SDL_image (if
available). `FILE` is the null-terminated path to the image file. `ISTAT` is set
to `0` if the image has been loaded successfully.

### SUBROUTINE GLOCK()
Locks current layer for direct pixel manipulation.

### SUBROUTINE GMOUSE(IXREL, IYREL)
Returns relative mouse motion as 8-byte integer.

### SUBROUTINE GOPEN(IW, IH, TITLE, ISTAT)
Opens the SDL 1.2 window. Only a single window can be opened at a time. `TITLE`
is the null-terminated window title. `ISTAT` is set to `0` if the window was
opened successfully.

### SUBROUTINE GPAL(N)
Allocates palette memory for `N` colours.

### SUBROUTINE GPIXEL(IX, IY)
Draws single pixel on layer. The surface must be locked.

### SUBROUTINE GSETP(I, IR, IG, IB)
Sets RGB palette colour at index `I` in screen format to given RGB values.

### SUBROUTINE GSSHOT(FILE)
Copies screen surface in BMP format to given file (screen shot). The file path
must be null-terminated.

### SUBROUTINE GULOCK()
Unlocks layer surface.

### SUBROUTINE GVIDEO(IHW, IWM, IVM)
Returns status information (hardware acceleration, window manager, video
memory).

### SUBROUTINE GVLINE(IX, IY1, IY2)
Draws vertical line to layer surface. No bounds checking!

### SUBROUTINE GWARP(IX, IY)
Warps mouse to `IX`, `IY`.

### SUBROUTINE MCHAN(I, ICHAN, LOOPS)
Plays sound effect `I` in chunk array on channel `ICHAN`. Set the channel to
`-1` to use the next free. `LOOPS` sets the number of loops to play for.

### SUBROUTINE MCLOSE()
Closes music file.

### SUBROUTINE MHALT()
Stops music playback.

### SUBROUTINE MLOAD(FILE, ISTAT)
Loads music file (preferable, in OGG format). `ISTAT` is set to `0` if the file
has been loaded successfully.

### SUBROUTINE MLOADW(I, FILE, ISTAT)
Loads WAV file to position `I` in audio chunk array. Up to eight sound effects
may be loaded (from `0` to `7`). `ISTAT` is set to `0` if the file has been
loaded successfully.

### SUBROUTINE MPAUSE()
Pauses music playback.

### SUBROUTINE MPLAY(LOOPS)
Plays music file. `LOOPS` sets the number of loops to play for, `0` means “play
once and stop”.
