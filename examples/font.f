C     ******************************************************************
C
C     FONT
C
C     SDL77 PROGRAM THAT SHOWS BITMAP FONT RENDERING. THE USED FONT IS
C     "DOS/V RE. JPN24" IN 12 X 24 PIXELS.
C
C     THE EXAMPLE LOADS IMAGE FILES FROM LOCAL DIRECTORY "../SHARE/".
C
C     DATE..: 2022-08-28
C     AUTHOR: PHILIPP ENGEL
C
C     ******************************************************************
      PROGRAM DEMO
      EXTERNAL  FTEXT, INIT
      EXTERNAL  GBLIT, GCLOSE, GCUR, GEVENT, GFLUSH, GLAYER
      INTEGER   GKEY
      INTEGER*8 GTICKS

      INCLUDE 'const.fi'
      INCLUDE 'event.fi'
      INCLUDE 'key.fi'
      INTEGER IMGW, IMGH, MAXFPS
      REAL    FT
      PARAMETER (IMGW=100, IMGH=100, MAXFPS=30, FT=1.0/MAXFPS*1000)

      INTEGER   IDELAY, IDT, IDXY, IEVENT, ISTAT
      INTEGER*8 IT
      LOGICAL   DONE
      DATA DONE, IDXY /.FALSE.,0/
C
C     OPEN SDL 1.2 WINDOW AND INITIALISE THE BITMAP FONT.
C
      CALL INIT(IW, IH, IMGW, IMGH, ISTAT)
      IF (ISTAT .NE. 0) STOP
      CALL GCUR(0)
C
C     MAIN LOOP, RUNS AT 30 FPS.
C
   10 CONTINUE
      IT = GTICKS()
      IF (GKEY(KESC) .EQ. 1) DONE = .TRUE.

   20 CONTINUE
      CALL GEVENT(IEVENT, ISTAT)
      IF (IEVENT .EQ. EQUIT) DONE = .TRUE.
      IF (ISTAT .EQ. 1) GOTO 20

      CALL GLAYER(0)
      CALL GBLIT(1, IDXY, IDXY, 0, 0, IW, IH)
      CALL FTEXT(304, 128, 'FORTRAN FOREVER!')
      CALL FTEXT(304, 176, 'FORTRAN FOREVER!')
      CALL FTEXT(304, 224, 'FORTRAN FOREVER!')
      IDXY = MOD(IDXY + 1, IMGW)

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
      INTEGER FUNCTION LTRIM(STR)
C
C     RETURNS LENGTH OF TRIMMED STRING, LIKE LEN_TRIM() IN FORTRAN 90.
C
      CHARACTER*(*) STR

      DO 10, LTRIM = LEN(STR), 1, -1
      IF (STR(LTRIM:LTRIM) .NE. ' ') RETURN
   10 CONTINUE
      END
C     ******************************************************************
      SUBROUTINE INIT(IW, IH, IMGW, IMGH, ISTAT)
C
C     OPENS SDL 1.2 WINDOW AND CREATES ALL LAYERS.
C
      EXTERNAL FINIT, GALLOC, GBLIT, GCREAT, GLAYER, GLOAD, GOPEN

      INTEGER IW, IH, IMGW, IMGH, ISTAT
      INTEGER I, J, IX, IY

      CALL GOPEN(IW, IH, 'FORTRAN' // ACHAR(0), ISTAT)
      IF (ISTAT .NE. 0) RETURN
C
C     CREATE BUFFER LAYER.
C
      CALL GLAYER(1)
      CALL GCREAT(IW + IMGW, IH + IMGH)
      CALL GALLOC(ISTAT)
      IF (ISTAT .NE. 0) RETURN
C
C     LOAD BACKGROUND IMAGE TO LAYER.
C
      CALL GLAYER(2)
      CALL GLOAD('share/back.png' // ACHAR(0), ISTAT)
      IF (ISTAT .NE. 0) RETURN
C
C     LOAD FONT IMAGE TO LAYER AND INITIALISE BITMAP FONT.
C
      CALL GLAYER(3)
      CALL GLOAD('share/font.png' // ACHAR(0), ISTAT)
      IF (ISTAT .NE. 0) RETURN
      CALL FINIT(3, 12, 24, 32)
C
C     FILL BUFFER LAYER (1) WITH BACKGROUND LAYER (2).
C
      CALL GLAYER(1)
      DO 10 I = 0, IW / IMGW
      DO 20 J = 0, IH / IMGH
      IX = I * IMGW
      IY = J * IMGH
      CALL GBLIT(2, 0, 0, IX, IY, IMGW, IMGH)
   20 CONTINUE
   10 CONTINUE
      END
C     ******************************************************************
      SUBROUTINE FINIT(LAYER, ICHARW, ICHARH, NCOLS)
C
C     SETS FONT LAYER, FONT CHARACTER SIZE, AND NUMBER OF COLUMNS IN
C     BITMAP FONT.
C
      INTEGER LAYER, ICHARW, ICHARH, NCOLS
      INTEGER L, IW, IH, N
      COMMON /FONT/ L, IW, IH, N

      L  = LAYER
      IW = ICHARW
      IH = ICHARH
      N  = NCOLS
      END
C     ******************************************************************
      SUBROUTINE FTEXT(IX, IY, STR)
C
C     PRINTS TEXT TO SCREEN LAYER (0).
C
      EXTERNAL GBLIT, GLAYER
      INTEGER  LTRIM

      INTEGER       IX, IY
      CHARACTER*(*) STR

      INTEGER ICX, ICY, I, J, K
      INTEGER IW, IH, L, N
      COMMON /FONT/ L, IW, IH, N

      CALL GLAYER(0)
      DO 10 I = 1, LTRIM(STR)
      J = IACHAR(STR(I:I)) - 32
      K = (I - 1) * IW
      ICX = IW * MOD(J, N)
      ICY = IH * (J / N)
      CALL GBLIT(L, ICX, ICY, IX + K, IY, IW, IH)
   10 CONTINUE
      END
