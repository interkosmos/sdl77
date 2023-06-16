C     ******************************************************************
C
C     ARCHI
C
C     DRAWS ARCHIMEDEAN SPIRAL:
C
C       https://rosettacode.org/wiki/Archimedean_spiral
C
C     DATE..: 2023-06-16
C     AUTHOR: PHILIPP ENGEL
C
C     ******************************************************************
      PROGRAM ARCHI
      EXTERNAL GCLOSE, GCOLOR, GDELAY, GEVENT, GFLUSH, GOPEN
      EXTERNAL SPIRAL
      INTEGER  GKEY

      INCLUDE 'event.fi'
      INCLUDE 'key.fi'
      INTEGER IDELAY, IW, IH
      PARAMETER (IDELAY=20, IW=800, IH=800)

      INTEGER IEVENT, ISTAT
      LOGICAL DONE

      DATA DONE /.FALSE./
C
C     OPEN SDL 1.2 WINDOW.
C
      CALL GOPEN(IW, IH, 'FORTRAN' // ACHAR(0), ISTAT)
      IF (ISTAT .NE. 0) STOP
C
C     RENDER THE SPIRAL.
C
      CALL GCOLOR(255, 255, 255)
      CALL SPIRAL(IW, IH)
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
      SUBROUTINE SPIRAL(IW, IH)
C
C     DRAWS ARCHIMEDEAN SPIRAL TO SCREEN LAYER.
C
      EXTERNAL GLINE

      INTEGER IW, IH

      DOUBLE PRECISION OFF, PI, SP, STEP
      PARAMETER (OFF=3.0, PI=ACOS(-1.0D0), SP=1.5, STEP=0.1)

      DOUBLE PRECISION R, THETA
      INTEGER ICX, ICY, X, Y, XX, YY

      ICX = IW / 2
      ICY = IH / 2

      X = ICX
      Y = ICY

      THETA = 0.0

   10 CONTINUE
      R = OFF + (SP * THETA)
      XX = INT(R * COS(THETA)) + ICX
      YY = INT(R * SIN(THETA)) + ICY
      CALL GLINE(X, Y, XX, YY)
      X = XX
      Y = YY
      THETA = THETA + STEP
      IF (THETA .LT. 60 * PI) GOTO 10
      END
