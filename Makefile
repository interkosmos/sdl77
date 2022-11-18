.POSIX:

CC      = gcc
FC      = gfortran
AR      = ar
CFLAGS  = -O2 -ffast-math -march=native -Wall -fpic
FFLAGS  = -O2 -ffast-math -march=native -Wall -std=legacy -fimplicit-none
AFLAGS  = rcs
LDFLAGS = `sdl-config --cflags`
TARGET  = libSDL77.a
LDLIBS  = $(TARGET) `sdl-config --libs` -lSDL_image -lSDL_mixer

.PHONY: all clean examples noimage nolibs nomixer

all: $(TARGET)

$(TARGET):
	$(CC) $(CFLAGS) $(LDFLAGS) -c src/SDL77.c
	$(AR) $(AFLAGS) $(TARGET) SDL77.o

examples: $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o engine examples/engine.f $(LDLIBS)
	$(FC) $(FFLAGS) $(LDFLAGS) -o fern examples/fern.f $(LDLIBS)
	$(FC) $(FFLAGS) $(LDFLAGS) -o fire examples/fire.f $(LDLIBS)
	$(FC) $(FFLAGS) $(LDFLAGS) -o fizzle examples/fizzle.f $(LDLIBS)
	$(FC) $(FFLAGS) $(LDFLAGS) -o font examples/font.f $(LDLIBS)
	$(FC) $(FFLAGS) $(LDFLAGS) -o mode7 examples/mode7.f $(LDLIBS)
	$(FC) $(FFLAGS) $(LDFLAGS) -o root3 examples/root3.f $(LDLIBS)
	$(FC) $(FFLAGS) $(LDFLAGS) -o shuttle examples/shuttle.f $(LDLIBS)
	$(FC) $(FFLAGS) $(LDFLAGS) -o smoke examples/smoke.f $(LDLIBS)
	$(FC) $(FFLAGS) $(LDFLAGS) -o track examples/track.f $(LDLIBS)

noimage:
	$(CC) $(CFLAGS) -DNO_IMAGE $(LDFLAGS) -c src/SDL77.c
	$(AR) $(AFLAGS) $(TARGET) SDL77.o

nolibs:
	$(CC) $(CFLAGS) -DNO_IMAGE -DNO_MIXER $(LDFLAGS) -c src/SDL77.c
	$(AR) $(AFLAGS) $(TARGET) SDL77.o

nomixer:
	$(CC) $(CFLAGS) -DNO_MIXER $(LDFLAGS) -c src/SDL77.c
	$(AR) $(AFLAGS) $(TARGET) SDL77.o

clean:
	if [ -e $(TARGET) ]; then rm $(TARGET); fi
	if [ -e engine ]; then rm engine; fi
	if [ -e fern ]; then rm fern; fi
	if [ -e fire ]; then rm fire; fi
	if [ -e fizzle ]; then rm fizzle; fi
	if [ -e font ]; then rm font; fi
	if [ -e mode7 ]; then rm mode7; fi
	if [ -e root3 ]; then rm root3; fi
	if [ -e shuttle ]; then rm shuttle; fi
	if [ -e smoke ]; then rm smoke; fi
	if [ -e track ]; then rm track; fi
	rm *.o
