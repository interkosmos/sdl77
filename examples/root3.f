C     ******************************************************************
C
C     ROOT 3
C
C     DRAWS COMPLEX CUBE ROOT FRACTAL. BASED ON CODE POSTED ON:
C     https://groups.google.com/g/comp.lang.fortran/c/jznLxC-Gue0
C
C     DATE..: 2022-09-02
C     AUTHOR: PHILIPP ENGEL
C
C     ******************************************************************
      PROGRAM ROOT3
      EXTERNAL PLOT
      EXTERNAL GCLOSE, GDELAY, GEVENT, GFLUSH, GOPEN
      INTEGER  GKEY

      INCLUDE 'event.fi'
      INCLUDE 'key.fi'
      INTEGER IDELAY, IW, IH
      PARAMETER (IDELAY=100, IW=1024, IH=768)

      INTEGER IEVENT, ISTAT
      LOGICAL DONE
      DATA DONE /.FALSE./
C
C     OPEN SDL 1.2 WINDOW.
C
      CALL GOPEN(IW, IH, 'FORTRAN' // ACHAR(0), ISTAT)
      IF (ISTAT .NE. 0) STOP
C
C     DRAW TO SCREEN LAYER.
C
      CALL GLAYER(0)
      CALL PLOT(IW, IH)
C
C     MAIN LOOP.
C
   10 CONTINUE
   20 CONTINUE
      CALL GEVENT(IEVENT, ISTAT)
      IF (IEVENT .EQ. EQUIT) DONE = .TRUE.
      IF (ISTAT .EQ. 1) GOTO 20
      IF (GKEY(KESC) .EQ. 1) DONE = .TRUE.
      CALL GFLUSH()
      CALL GDELAY(IDELAY)
      IF (.NOT. DONE) GOTO 10
C
C     CLEAN UP AND QUIT.
C
      CALL GCLOSE()
      END
C     ******************************************************************
      SUBROUTINE PLOT(COLS, ROWS)
C
C     DRAWS THE FRACTAL TO CURRENT LAYER. USES THE GNU EXTENSIONS
C     DCMPLX() AND DIMAG() WHICH CAN BE SUBSTITUTED WITH THEIR MODERN
C     FORTRAN COUNTERPARTS (IF AVAILABLE).
C
      INTRINSIC DCMPLX, DIMAG
      EXTERNAL  GCOLOR, GLOCK, GPIXEL, GULOCK

      DOUBLE PRECISION CONST, R2R
      PARAMETER (CONST=1.905D-3, R2R=0.4D0)

      INTEGER COLS, ROWS

      DOUBLE PRECISION ANGLE
      DOUBLE COMPLEX   D, W, Z
      INTEGER          I, IX, IY, J

      CALL GLOCK()

      DO 10 IX = -COLS/2, COLS - COLS/2 - 1
      DO 20 IY = -ROWS/2, ROWS - ROWS/2 - 1
      ANGLE = 0D0
      IF (IX .EQ. 0 .AND. IY .EQ. 0) GOTO 50
      Z = DCMPLX(IX * CONST, IY * CONST)
      DO 30 I = 1, 224
      W = Z**2
      Z = Z * W * 2 + 1
      Z = Z / W / 3
      D = Z - 1
      IF (ABS(D) .LT. R2R) GOTO 40
      D = Z + DCMPLX(0.5D0, 0.866D0)
      IF (ABS(D) .LT. R2R) GOTO 40
      D = Z + DCMPLX(0.5D0, -0.866D0)
      IF (ABS(D) .LT. R2R) GOTO 40
   30 CONTINUE
   40 CONTINUE
      ANGLE = ATAN2(DIMAG(D), DBLE(D))
   50 CONTINUE
      J = INT(ABS(ANGLE / 0.012272D0))
      CALL GCOLOR(J, J, J)
      CALL GPIXEL(IX + COLS/2, IY + ROWS/2)
   20 CONTINUE
   10 CONTINUE

      CALL GULOCK()
      END
