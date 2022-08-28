.POSIX:

CC      = gcc
FC      = gfortran
AR      = ar
CFLAGS  = -O3 -ffast-math -Wall -march=native -fpic
FFLAGS  = -O3 -ffast-math -Wall -fimplicit-none
AFLAGS  = rcs
LDFLAGS = `sdl-config --cflags`
TARGET  = libSDL77.a
LDLIBS  = $(TARGET) `sdl-config --libs` -lSDL_image -lSDL_mixer

.PHONY: all clean examples

all: $(TARGET)

$(TARGET):
	$(CC) $(CFLAGS) $(LDFLAGS) -c src/SDL77.c
	$(AR) $(AFLAGS) $(TARGET) SDL77.o

examples: $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o track examples/track.f $(LDLIBS)
	$(FC) $(FFLAGS) $(LDFLAGS) -o fire examples/fire.f $(LDLIBS)
	$(FC) $(FFLAGS) $(LDFLAGS) -o fizzle examples/fizzle.f $(LDLIBS)
	$(FC) $(FFLAGS) $(LDFLAGS) -o font examples/font.f $(LDLIBS)
	$(FC) $(FFLAGS) $(LDFLAGS) -o mode7 examples/mode7.f $(LDLIBS)
	$(FC) $(FFLAGS) $(LDFLAGS) -o shuttle examples/shuttle.f $(LDLIBS)

clean:
	if [ -e $(TARGET) ]; then rm $(TARGET); fi
	if [ -e track ]; then rm track; fi
	if [ -e fire ]; then rm fire; fi
	if [ -e fizzle ]; then rm fizzle; fi
	if [ -e font ]; then rm font; fi
	if [ -e mode7 ]; then rm mode7; fi
	if [ -e shuttle ]; then rm shuttle; fi
	rm *.o
