C     ******************************************************************
C
C     FLOWER
C
C     DRAWS SUNFLOWER FRACTAL:
C
C       https://rosettacode.org/wiki/Sunflower_fractal
C
C     DATE..: 2023-06-16
C     AUTHOR: PHILIPP ENGEL
C
C     ******************************************************************
      PROGRAM FLOWER
      EXTERNAL GCLOSE, GDELAY, GEVENT, GFLUSH, GOPEN
      EXTERNAL SUNFLO
      INTEGER  GKEY

      INCLUDE 'event.fi'
      INCLUDE 'key.fi'
      INTEGER IDELAY
      PARAMETER (IDELAY=20)

      INTEGER IEVENT, ISTAT
      LOGICAL DONE

      DATA DONE /.FALSE./
C
C     OPEN SDL 1.2 WINDOW.
C
      CALL GOPEN(800, 800, 'FORTRAN' // ACHAR(0), ISTAT)
      IF (ISTAT .NE. 0) STOP
C
C     RENDER THE FRACTAL.
C
      CALL SUNFLO(12000)
C
C     MAIN LOOP. CHECKS INPUT AND COPIES SCREEN LAYER TO SCREEN.
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
      SUBROUTINE SUNFLO(NSEEDS)
C
C     DRAWS SUNFLOWER FRACTAL TO SCREEN LAYER.
C
      EXTERNAL GCIRC, GCOLOR

      INTEGER NSEEDS

      DOUBLE PRECISION C, PI
      PARAMETER (C=(SQRT(5.0D0)+1.0D0)/2.0D0, PI=ACOS(-1.0D0))

      DOUBLE PRECISION A, F, R, X, Y
      INTEGER I

      CALL GCOLOR(255, 255, 0)

      DO 10 I = 1, NSEEDS
      F = DBLE(I)
      R = (F**C) / NSEEDS
      A = 2 * PI * C * F
      X = R * SIN(A) + 400
      Y = R * COS(A) + 400
      F = F / (NSEEDS / 5.0D0)
      CALL GCIRC(INT(X), INT(Y), INT(F))
   10 CONTINUE
      END
