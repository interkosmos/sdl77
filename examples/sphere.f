C     ******************************************************************
C
C     SPHERE
C
C     DRAWS A SPHERE. PORTED FROM THE FOLLOWING ROSETTA CODE TASK:
C
C       https://rosettacode.org/wiki/Draw_a_sphere
C
C     DATE..: 2022-11-18
C     AUTHOR: PHILIPP ENGEL
C
C     ******************************************************************
      PROGRAM SPHERE
      EXTERNAL GCLOSE, GDELAY, GEVENT, GFLUSH, GOPEN
      EXTERNAL NORM, RENDER
      INTEGER  GKEY

      INCLUDE 'const.fi'
      INCLUDE 'event.fi'
      INCLUDE 'key.fi'
      INTEGER IDELAY
      PARAMETER (IDELAY=100)

      INTEGER IEVENT, ISTAT
      LOGICAL DONE
      REAL    DIR(3)

      DATA DONE /.FALSE./
      DATA DIR  /-30.0,-30.0,50.0/
C
C     OPEN SDL 1.2 WINDOW.
C
      CALL GOPEN(IW, IH, 'FORTRAN' // ACHAR(0), ISTAT)
      IF (ISTAT .NE. 0) STOP
C
C     RENDER THE SCENE.
C
      CALL NORM(DIR)
      CALL RENDER(IW, IH, 200, 1.5, 0.2, DIR)
C
C     MAIN LOOP. CHECKS INPUT AND COPIES SCREEN LAYER TO SCREEN.
C
   10 CONTINUE
   20 CONTINUE
      CALL GEVENT(IEVENT, ISTAT)
      IF (IEVENT .EQ. IEQUIT) DONE = .TRUE.
      IF (ISTAT .EQ. 1) GOTO 20
      IF (GKEY(IESC) .EQ. 1) DONE = .TRUE.
      CALL GFLUSH()
      CALL GDELAY(IDELAY)
      IF (.NOT. DONE) GOTO 10
C
C     CLEAN UP AND QUIT.
C
      CALL GCLOSE()
      END
C     ******************************************************************
      REAL FUNCTION DOT(X, Y)
C
C     RETURNS DOT PRODUCT.
C
      REAL X(3), Y(3)

      DOT = X(1) * Y(1) + X(2) * Y(2) + X(3) * Y(3)
      END
C     ******************************************************************
      SUBROUTINE NORM(V)
C
C     NORMALIZES VECTOR.
C
      REAL DOT
      REAL V(3)
      REAL X

      X = 1 / SQRT(DOT(V, V))
      V(1) = V(1) * X
      V(2) = V(2) * X
      V(3) = V(3) * X
      END
C     ******************************************************************
      SUBROUTINE RENDER(IW, IH, IR, K, AMB, DIR)
C
C     DRAWS THE SPHERE.
C
C     ARGUMENTS:
C
C       IW      -   SCREEN WIDTH.
C       IH      -   SCREEN HEIGHT.
C       IR      -   SPHERE RADIUS.
C       K       -   SPOT LIGHT.
C       AMB     -   REFLECTIVE LIGHT (AMBIENT).
C       DIR     -   LIGHT DIRECTION.
C
      EXTERNAL GCOLOR, GLAYER, GLOCK, GPIXEL, GULOCK
      EXTERNAL NORM
      REAL     DOT

      INTEGER IW, IH, IR
      REAL    K, AMB, DIR(3)

      INTEGER IW2, IH2, IX, IY, IZ, LUM
      REAL    S, VEC(3)

      IW2 = IW / 2
      IH2 = IH / 2

      CALL GLAYER(0)
      CALL GLOCK()

      DO 10 IX = -IR, IR
      DO 20 IY = -IR, IR
      IZ = IR**2 - IX**2 - IY**2
      IF (IZ .GE. 0) THEN
        VEC(1) = REAL(IX)
        VEC(2) = REAL(IY)
        VEC(3) = SQRT(REAL(IZ))
        CALL NORM(VEC)
        S = MAX(0.0, DOT(DIR, VEC))
        LUM = NINT(255 * (S**K + AMB) / (1 + AMB))
        LUM = MAX(0, MIN(LUM, 255))
        CALL GCOLOR(LUM, LUM, LUM)
        CALL GPIXEL(IW2 + IX, IH2 + IY)
      END IF
   20 CONTINUE
   10 CONTINUE

      CALL GULOCK()
      END
