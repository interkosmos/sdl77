C     ******************************************************************
C
C     BARNSLEY FERN
C
C     EXAMPLE PROGRAM THAT PLOTS THE BARNSLEY FERN, A FRACTAL THAT CAN
C     BE CREATED USING AN INTERATED FUNCTION SYSTEM.
C
C     DATE..: 2022-08-30
C     AUTHOR: PHILIPP ENGEL
C
C     ******************************************************************
      PROGRAM FERN
      EXTERNAL BARNS, PLOT
      EXTERNAL GCLOSE, GCUR, GDELAY, GEVENT, GFLUSH, GOPEN
      INTEGER  GKEY, GTIME

      INCLUDE 'const.fi'
      INCLUDE 'event.fi'
      INCLUDE 'key.fi'
      INTEGER IDELAY, N
      REAL    SCALE
      PARAMETER (IDELAY=100, N=100000, SCALE=50.0)

      INTEGER IEVENT, ISTAT
      LOGICAL DONE
      REAL    XY(N), YY(N)
      DATA DONE /.FALSE./

      CALL SRAND(GTIME())
C
C     OPEN SDL 1.2 WINDOW AND HIDE CURSOR.
C
      CALL GOPEN(IW, IH, 'FORTRAN' // ACHAR(0), ISTAT)
      IF (ISTAT .NE. 0) STOP
      CALL GCUR(0)
C
C     CREATE AND DRAW FRACTAL.
C
      CALL BARNS(N, XY, YY)
      CALL PLOT(IW/2, 25, IW, IH, N, XY, YY, SCALE)
C
C     MAIN LOOP.
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
      SUBROUTINE BARNS(N, XY, YY)
C
C     CALCULATES FRACTAL.
C
      REAL RAND

      INTEGER N
      REAL    XY(N), YY(N)

      REAL P(4), A(4), B(4), C(4), D(4), E(4), F(4)
      COMMON /FRACT/ P, A, B, C, D, E, F

      INTEGER I
      REAL    R, X, Y
      DATA X, Y /0.0,0.0/

      DO 10 I = 1, N
      R = RAND()
      IF (R .LT. P(1)) THEN
        X = A(1) * X + B(1) * Y + E(1)
        Y = C(1) * X + D(1) * Y + F(1)
      ELSE IF (R .LT. (P(1) + P(2))) THEN
        X = A(2) * X + B(2) * Y + E(2)
        Y = C(2) * X + D(2) * Y + F(2)
      ELSE IF (R .LT. (P(1) + P(2) + P(3))) THEN
        X = A(3) * X + B(3) * Y + E(3)
        Y = C(3) * X + D(3) * Y + F(3)
      ELSE
        X = A(4) * X + B(4) * Y + E(4)
        Y = C(4) * X + D(4) * Y + F(4)
      END IF
      XY(I) = X
      YY(I) = Y
   10 CONTINUE
      END
C     ******************************************************************
      SUBROUTINE PLOT(IX, IY, IW, IH, N, XY, YY, SCALE)
C
C     PLOTS FRACTAL.
C
      EXTERNAL GCOLOR, GFILL, GLAYER, GLOCK, GPIXEL, GULOCK

      INTEGER IX, IY, IW, IH, N
      REAL    XY(N), YY(N), SCALE

      INTEGER I, J, K

      CALL GLAYER(0)
      CALL GCOLOR(0, 0, 0)
      CALL GFILL()
      CALL GCOLOR(0, 127, 0)
      CALL GLOCK()

      DO 10 I = 1, N
      J = INT(XY(I) * SCALE) + IX
      K = IH - INT(YY(I) * SCALE) - IY
      IF (J .GE. 0 .AND. J .LT. IW .AND.
     &    K .GE. 0 .AND. K .LT. IH) CALL GPIXEL(J, K)
   10 CONTINUE

      CALL GULOCK()
      END
C     ******************************************************************
      BLOCK DATA
C
C     COMMON VARIABLES:
C
C     P                - PROBABILITIES
C     A, B, C, D, E, F - COEFFICIENTS
C
      REAL P(4), A(4), B(4), C(4), D(4), E(4), F(4)
      COMMON /FRACT/ P, A, B, C, D, E, F
      DATA P /0.01,0.85,0.07,0.07/
      DATA A /0.00,0.85,0.20,-0.15/
      DATA B /0.00,0.04,-0.26,0.28/
      DATA C /0.00,-0.04,0.23,0.26/
      DATA D /0.16,0.85,0.22,0.24/
      DATA E /0.00,0.00,0.00,0.00/
      DATA F /0.00,1.60,1.60,0.44/
      END
