# SDL77
SDL77 is a C library for game development in FORTRAN 77 that provides
some glue code to access the software renderer of SDL 1.2. Common
[FORTRAN/C calling conventions](https://www.math.utah.edu/software/c-with-fortran.html)
are used for mixed-language programming (compatible to f77, f2c, g77,
GNU Fortran, DEC/Compaq, Portland Group, SGI, Sun, …). For modern
Fortran 2008 interface bindings to SDL 2.0, see
[fortran-sdl2](https://github.com/interkosmos/fortran-sdl2).

The library has the following dependencies:

* SDL 1.2
* SDL_image
* SDL_mixer

You may have to install additional development headers.

## Build Instructions
On FreeBSD, first install the dependencies:

```
# pkg install audio/sdl_mixer devel/sdl12 graphics/sdl_image
```

Then, build `libSDL77.a` with GCC by executing the provided Makefile:

```
$ make
```

You may want to compile the library without the SDL_image dependency
(restricting the supported image formats to BMP only):

```
$ make noimage
```

To build without SDL_mixer, run:

```
$ make nomixer
```

And only bare SDL 1.2, without SDL_image and SDL_mixer:

```
$ make nolibs
```

Link your FORTRAN 77 program against `libSDL77.a -lSDL -lSDL_image -lSDL_mixer`.
Alternatively, you can simply link with object file `SDL77.o` instead of
`libSDL77.a`.

## Example
The following example program `demo.f` in ANSI FORTRAN 77 opens an SDL
1.2 window and fills a green rectangle.

```fortran
C     ******************************************************************
C
C     SDL77 DEMO PROGRAM IN FORTRAN 77.
C
C     ******************************************************************
      PROGRAM DEMO
C
C     EXTERNAL PROCEDURES.
C
      EXTERNAL GCLOSE, GOPEN
      EXTERNAL GCOLOR, GDELAY, GEVENT, GFILLR, GFLUSH, GLAYER
      INTEGER  GKEY
C
C     PARAMETERS.
C
      INTEGER IDELAY, IESC, IEQUIT, IW, IH
      PARAMETER (IDELAY=50, IESC=27, IEQUIT=12, IW=640, IH=480)
C
C     VARIABLES.
C
      INTEGER IEVENT, ISTAT
      LOGICAL DONE
      DATA DONE /.FALSE./
C
C     OPEN SDL 1.2 WINDOW.
C
      CALL GOPEN(IW, IH, 'FORTRAN' // ACHAR(0), ISTAT)
      IF (ISTAT .NE. 0) STOP
C
C     MAIN LOOP: POLLS EVENTS, FILLS RECTANGLE, FLIPS BUFFER TO SCREEN.
C
   10 CONTINUE
C
C     PROCESS EVENTS.
C
   20 CONTINUE
      CALL GEVENT(IEVENT, ISTAT)
      IF (IEVENT .EQ. IEQUIT) DONE = .TRUE.
      IF (ISTAT .EQ. 1) GOTO 20
C
C     PROCESS KEYBOARD INPUT.
C
      IF (GKEY(IESC) .EQ. 1) DONE = .TRUE.
C
C     FILL RECTANGLE.
C
      CALL GLAYER(0)
      CALL GCOLOR(0, 255, 0)
      CALL GFILLR(50, 50, 150, 150)
C
C     FLIP TO SCREEN.
C
      CALL GFLUSH()
      CALL GDELAY(IDELAY)
      IF (.NOT. DONE) GOTO 10
C
C     QUIT.
C
      CALL GCLOSE()
      END
```

Link the demo program against SDL77, SDL, SDL_image, and SDL_mixer:

```
$ gfortran -o demo demo.f libSDL77.a -lSDL -lSDL_image -lSDL_mixer
$ ./demo
```

The code is compatible to f2c as well:

```
$ f2c demo.f
demo.f:
   MAIN demo:
$ gcc -o demo demo.c libSDL77.a -lSDL -lSDL_image -lSDL_mixer -lf2c -lm
$ ./demo
```

## Further Examples
Some example programs can be found in directory `examples/`:

* **bship** draws burning ship fractal.
* **engine** renders a 2.5-D environment through ray-casting.
* **fern** draws a Barnsley fern fractal.
* **fire** renders the DOOM fire effect.
* **fizzle** demonstrates a fizzle-fade effect based on Fisher-Yates shuffle.
* **font** prints text with a bitmap font.
* **mode7** shows affine transformation for perspective correction.
* **root3** is another fractal demo.
* **shuttle** renders the wireframe model of a space shuttle.
* **smoke** renders a 3-D chaotic attractor.
* **sphere** draws a shaded sphere.
* **track** plays an audio track in OGG format.

Build the examples with:

```
$ make examples
```

## Licence
ISC
