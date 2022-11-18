C     ******************************************************************
C
C     MODE 7
C
C     EXAMPLE THAT SHOWS BASIC PERSPECTIVE CORRECTION VIA AFFINE
C     TRANSFORMATION, LIKE MODE 7 ON THE SNES.
C
C     DATE..: 2022-08-28
C     AUTHOR: PHILIPP ENGEL
C
C     ******************************************************************
      PROGRAM MODE7
      EXTERNAL INIT, RENDER, UPDATE
      EXTERNAL GCLOSE, GCUR, GEVENT, GFLUSH
      INTEGER  GKEY, GTICKS

      INCLUDE 'event.fi'
      INCLUDE 'key.fi'
      INTEGER MAXFPS
      REAL    FT
      PARAMETER (MAXFPS=15, FT=1.0/MAXFPS*1000)

      INTEGER IW, IH
      COMMON /WINDOW/ IW, IH

      INTEGER IDELAY, IDT, IEVENT, ISTAT, IT
      LOGICAL DONE
      DATA DONE /.FALSE./
C
C     OPEN SDL 1.2 WINDOW.
C
      CALL INIT(IW, IH, ISTAT)
      IF (ISTAT .NE. 0) STOP
      CALL GCUR(0)
C
C     MAIN LOOP.
C
   10 CONTINUE
      IT = GTICKS()
   20 CONTINUE
      CALL GEVENT(IEVENT, ISTAT)
      IF (IEVENT .EQ. IEQUIT) DONE = .TRUE.
      IF (ISTAT .EQ. 1) GOTO 20
      IF (GKEY(IESC) .EQ. 1) DONE = .TRUE.

      CALL UPDATE()
      CALL RENDER()
      CALL GFLUSH()

      IDT = GTICKS() - IT
      IDELAY = MAX(0, INT(FT - IDT))
      IF (IDELAY .GT. 0) CALL GDELAY(IDELAY)
      IF (.NOT. DONE) GOTO 10
C
C     CLEAN UP AND QUIT.
C
      CALL GCUR(1)
      CALL GCLOSE()
      END
C     ******************************************************************
      INTEGER FUNCTION ABSMOD(A, P)
C
C     ABSOLUTE MODULO FUNCTION.
C
      INTEGER A, P
      ABSMOD = MOD(MOD(A, P) + P, P)
      END
C     ******************************************************************
      SUBROUTINE INIT(IW, IH, ISTAT)
C
C     OPENS SDL 1.2 WINDOW AND LOADS TEXTURE.
C
      EXTERNAL GCOLOR, GFILL, GLAYER, GLOAD, GOPEN
      INTEGER IW, IH, ISTAT
C
C     OPEN WINDOW AND FILL SCREEN LAYER.
C
      CALL GOPEN(IW, IH, 'FORTRAN' // ACHAR(0), ISTAT)
      IF (ISTAT .NE. 0) RETURN
      CALL GLAYER(0)
      CALL GCOLOR(127, 0, 255)
      CALL GFILL()
C
C     LOAD TEXTURE TO LAYER.
C
      CALL GLAYER(1)
      CALL GLOAD('share/mode7.png' // ACHAR(0), ISTAT)
      IF (ISTAT .NE. 0) RETURN
      END
C     ******************************************************************
      SUBROUTINE RENDER()
C
C     RENDERS THE SCENE.
C
      EXTERNAL GCPPIX, GLOCK, GULOCK
      INTEGER  ABSMOD

      INTEGER IW, IH, IMGW, IMGH, IHZ
      INTEGER I, IW2, IH2, IX, IY, J
      REAL    ALPHA, X, Y, F, FOV
      REAL    PX, PY, PZ, RX, RY, SX, SY

      COMMON /WINDOW/ IW, IH
      COMMON /SCENE/  IMGW, IMGH, IHZ, F, FOV
      COMMON /VIEW/   X, Y, ALPHA

      IW2 = IW / 2
      IH2 = IH / 2

      CALL GLAYER(0)
      CALL GLOCK()
      CALL GLAYER(1)

      DO 10 J = 0, IH2 - 1
      DO 20 I = -IW2, IW2 - 1
      PX = REAL(I)
      PY = FOV
      PZ = REAL(J + IHZ)
      SX = PX / PZ
      SY = PY / PZ
      RX = SX * COS(ALPHA) - SY * SIN(ALPHA)
      RY = SX * SIN(ALPHA) + SY * COS(ALPHA)
      IX = ABSMOD(INT(RX * F + X), IMGW - 1)
      IY = ABSMOD(INT(RY * F + Y), IMGH - 1)
      CALL GCPPIX(IX, IY, IW2 + I, IH2 + J)
   20 CONTINUE
   10 CONTINUE

      CALL GLAYER(0)
      CALL GULOCK()
      END
C     ******************************************************************
      SUBROUTINE UPDATE()
C
C     UPDATES VIEW POINT.
C
      INTEGER IDOWN, ILEFT, IRIGHT, IUP
      REAL    DY, DA
      PARAMETER (IDOWN=274, ILEFT=276, IRIGHT=275, IUP=273)
      PARAMETER (DY=1.5, DA=0.05)
      INTEGER GKEY

      REAL ALPHA, X, Y
      COMMON /VIEW/ X, Y, ALPHA

      IF (GKEY(IDOWN)  .EQ. 1) Y = Y - DY
      IF (GKEY(IUP)    .EQ. 1) Y = Y + DY
      IF (GKEY(IRIGHT) .EQ. 1) ALPHA = ALPHA - DA
      IF (GKEY(ILEFT)  .EQ. 1) ALPHA = ALPHA + DA
      END
C     ******************************************************************
      BLOCK DATA
C
C     COMMON VARIABLES:
C
C     IW    - SCREEN WIDTH.
C     IH    - SCREEN HEIGHT.
C     IMGW  - IMAGE WIDTH.
C     IMGH  - IMAGE HEIGHT.
C     IHZ   - RELATIVE POSITION OF HORIZON.
C     F     - TEXTURE SCALE FACTOR.
C     FOV   - FIELD OF VIEW.
C     X     - VIEW POINT X.
C     Y     - VIEW POINT Y.
C     ALPHA - VIEW POINT ANGLE.
C
      INTEGER IW, IH, IMGW, IMGH, IHZ
      REAL    X, Y, ALPHA, F, FOV

      COMMON /WINDOW/ IW, IH
      COMMON /SCENE/  IMGW, IMGH, IHZ, F, FOV
      COMMON /VIEW/   X, Y, ALPHA

      DATA IW, IH                  /640,480/
      DATA IMGW, IMGH, IHZ, F, FOV /128,128,20,50.0,240.0/
      DATA X, Y, ALPHA             /1.0,1.0,0.0/
      END
