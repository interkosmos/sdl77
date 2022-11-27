C     ******************************************************************
C
C     2.5-D ENGINE
C
C     BASIC RAY-CASTING ENGINE.
C
C     DATE..: 2022-08-28
C     AUTHOR: PHILIPP ENGEL
C
C     ******************************************************************
      PROGRAM ENGINE
      EXTERNAL  GCLOSE, GCUR, GDELAY, GEVENT, GFLUSH, GOPEN
      INTEGER   GKEY
      INTEGER*8 GTICKS

      INCLUDE 'const.fi'
      INCLUDE 'event.fi'
      INCLUDE 'key.fi'
      INTEGER MAXFPS
      REAL    FT
      PARAMETER (MAXFPS=30, FT=1.0/MAXFPS*1000)

      INTEGER   IDELAY, IDT, IEVENT, ISTAT
      INTEGER*8 IT
      LOGICAL   DONE
      DATA DONE /.FALSE./
C
C     OPEN SDL 1.2 WINDOW.
C
      CALL GOPEN(IW, IH, 'FORTRAN' // ACHAR(0), ISTAT)
      IF (ISTAT .NE. 0) STOP
      CALL GCUR(0)
C
C     MAIN LOOP.
C
   10 CONTINUE
      IT = GTICKS()
   20 CONTINUE
      CALL GEVENT(IEVENT, ISTAT)
      IF (IEVENT .EQ. EQUIT) DONE = .TRUE.
      IF (ISTAT .EQ. 1) GOTO 20
      IF (GKEY(KESC) .EQ. 1) DONE = .TRUE.

      CALL UPDATE()
      CALL RENDER()
      CALL GFLUSH()

      IDT = INT(GTICKS() - IT)
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
      SUBROUTINE MOVE(SPEED)
C
C     MOVES PLAYER IN DX, DY.
C
      INCLUDE 'const.fi'
      REAL    SPEED
      INTEGER MAPY(MW, MH)
      REAL    POSX, POSY, DX, DY, PLANEX, PLANEY
      REAL    NX, NY
      COMMON /WORLD/ MAPY
      COMMON /STATE/ DX, DY, PLANEX, PLANEY, POSX, POSY

      NX = POSX + DX * SPEED
      NY = POSY + DY * SPEED

      IF (NX .LE. 1 .OR. NX .GE. MW .OR.
     &    NY .LE. 1 .OR. NY .GE. MH) RETURN

      IF (MAPY(INT(NX) + 1, INT(POSY) + 1) .EQ. 0) POSX = NX
      IF (MAPY(INT(POSX) + 1, INT(NY) + 1) .EQ. 0) POSY = NY
      END
C     ******************************************************************
      SUBROUTINE UPDATE()
C
C     MOVES/ROTATES PLAYER ON KEYBOARD INPUT.
C
      EXTERNAL MOVE, ROTATE
      INTEGER  GKEY

      INCLUDE 'const.fi'
      INCLUDE 'key.fi'
      REAL ALPHA, SPEED
      PARAMETER (ALPHA=0.1, SPEED=0.15)

      REAL POSX, POSY, DX, DY, PLANEX, PLANEY
      COMMON /STATE/ DX, DY, PLANEX, PLANEY, POSX, POSY

      IF (GKEY(KUP) .EQ. 1) THEN
C       MOVE FORWARD.
        CALL MOVE(SPEED)
      ELSE IF (GKEY(KDOWN) .EQ. 1) THEN
C       MOVE BACKWARD.
        CALL MOVE(-SPEED)
      ELSE IF (GKEY(KLEFT) .EQ. 1) THEN
C       TURN LEFT.
        CALL ROTATE(DX, DY, ALPHA)
        CALL ROTATE(PLANEX, PLANEY, ALPHA)
      ELSE IF (GKEY(KRIGHT) .EQ. 1) THEN
C       TURN RIGHT.
        CALL ROTATE(DX, DY, -ALPHA)
        CALL ROTATE(PLANEX, PLANEY, -ALPHA)
      END IF
      END
C     ******************************************************************
      SUBROUTINE RAYCST(IX)
C
C     RENDERS SINGLE COLUMN OF PIXELS AT IX.
C
      EXTERNAL GCOLOR, GVLINE
      INCLUDE 'const.fi'
      INTEGER IX

      INTEGER IRY(16), IGY(16), IBY(16), MAPY(MW, MH)
      REAL    POSX, POSY, DX, DY, PLANEX, PLANEY
      COMMON /PALET/ IRY, IGY, IBY
      COMMON /WORLD/ MAPY
      COMMON /STATE/ DX, DY, PLANEX, PLANEY, POSX, POSY

      INTEGER ISIDE, ISTEPX, ISTEPY, IWALL, LENGTH, MX, MY, IH2, IW2
      INTEGER IY1, IY2
      LOGICAL DONE
      REAL    CAMX, DIST, DDISTX, DDISTY, RAYX, RAYY
      REAL    SIDEX, SIDEY

      IW2 = IW / 2
      IH2 = IH / 2

      CAMX = 2 * IX / REAL(IW) - 1
      RAYX = DX + PLANEX * CAMX
      RAYY = DY + PLANEY * CAMX

      MX = INT(POSX)
      MY = INT(POSY)

      DDISTX = ABS(1 / RAYX)
      DDISTY = ABS(1 / RAYY)

      IF (RAYX .LT. 0) THEN
        ISTEPX = -1
        SIDEX  = (POSX - MX) * DDISTX
      ELSE
        ISTEPX = 1
        SIDEX  = (MX + 1.0 - POSX) * DDISTX
      END IF

      IF (RAYY .LT. 0) THEN
        ISTEPY = -1
        SIDEY  = (POSY - MY) * DDISTY
      ELSE
        ISTEPY = 1
        SIDEY  = (MY + 1.0 - POSY) * DDISTY
      END IF

      IWALL = 0
      DONE  = .FALSE.

   10 CONTINUE
      IF (SIDEX .LT. SIDEY) THEN
        SIDEX = SIDEX + DDISTX
        MX    = MX + ISTEPX
        ISIDE = 0
      ELSE
        SIDEY = SIDEY + DDISTY
        MY    = MY + ISTEPY
        ISIDE = 1
      END IF

      IF (MAPY(MX + 1, MY + 1) .GT. 0) THEN
        IWALL = MAPY(MX + 1, MY + 1)
        DONE  = .TRUE.
      END IF
      IF (.NOT. DONE) GOTO 10

      IF (IWALL .EQ. 0) RETURN

      IF (ISIDE .EQ. 0) THEN
        DIST = SIDEX - DDISTX
      ELSE
        DIST = SIDEY - DDISTY
      END IF

      IF (ISIDE .EQ. 1) IWALL = IWALL + 8

      LENGTH = INT(IH / DIST) / 2

      IY1 = MAX(0, -LENGTH + IH2 - 1)
      IY2 = MIN(IH - 1, LENGTH + IH2 - 1)

      CALL GCOLOR(IRY(IWALL), IGY(IWALL), IBY(IWALL))
      CALL GVLINE(IX, IY1, IY2)
      END
C     ******************************************************************
      SUBROUTINE RENDER()
C
C     RENDERS THE SCENE.
C
      EXTERNAL GCOLOR, GFILLR, GLAYER, RAYCST
      INCLUDE 'const.fi'
      INTEGER IX

      CALL GLAYER(0)

      CALL GCOLOR(0, 0, 0)
      CALL GFILLR(0, 0, IW, IH/2)
      CALL GCOLOR(31, 31, 31)
      CALL GFILLR(0, IH/2 - 1, IW, IH/2)

      DO 10 IX = 0, IW - 1
      CALL RAYCST(IX)
   10 CONTINUE
      END
C     ******************************************************************
      SUBROUTINE ROTATE(X, Y, A)
C
C     ROTATES X, Y BY ANGLE A [RAD].
C
      REAL X, Y, A
      REAL T

      T = X
      X = X * COS(A) - Y * SIN(A)
      Y = T * SIN(A) + Y * COS(A)
      END
C     ******************************************************************
      BLOCK DATA
C
C     COMMON DATA:
C
C     PALET - RGB COLOUR PALETTE.
C     STATE - GAME STATE (VECTORS).
C     WORLD - MAP DATA.
C
      INCLUDE 'const.fi'
      INTEGER IRY(16), IGY(16), IBY(16), MAPY(MW, MH)
      REAL    DX, DY, PLANEX, PLANEY, POSX, POSY

      COMMON /PALET/ IRY, IGY, IBY
      COMMON /STATE/ DX, DY, PLANEX, PLANEY, POSX, POSY
      COMMON /WORLD/ MAPY

      DATA DX, DY, PLANEX, PLANEY, POSX, POSY
     &/-1.0,0.0,0.0,0.66,10.5,10.5/
      DATA MAPY /02,07,07,03,07,03,07,03,07,03,07,01,
     &           02,00,00,00,00,00,00,00,00,00,00,01,
     &           02,00,00,00,00,00,00,00,00,00,00,02,
     &           04,00,07,02,00,00,00,00,00,00,00,01,
     &           02,00,07,00,00,00,00,00,00,00,00,02,
     &           04,00,00,00,02,02,02,00,00,00,00,01,
     &           02,00,00,00,02,03,03,00,00,00,00,02,
     &           02,00,00,00,00,00,00,00,00,00,00,01,
     &           02,00,00,00,00,00,00,00,00,05,00,02,
     &           02,00,04,04,04,00,00,00,00,05,00,01,
     &           02,00,00,00,00,00,00,00,00,00,00,02,
     &           02,06,06,06,05,05,05,04,04,03,03,01/
      DATA IRY, IGY, IBY
     &/255,127,  0,  0,255,127,  0,127,127, 63,  0, 0,127,63,  0, 63,
     &   0,255,255,127,127, 63,127,255,  0,127,127,63, 63,31, 63,127,
     &   0,  0,  0,  0, 63,127,255,  0,  0,  0,  0, 0, 31,63,127,  0/
      END
