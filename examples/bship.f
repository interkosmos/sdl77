C     ******************************************************************
C
C     BSHIP
C
C     DRAWS BURNING SHIP FRACTAL.
C
C     DATE..: 2022-11-18
C     AUTHOR: PHILIPP ENGEL
C
C     ******************************************************************
      PROGRAM BSHIP
      EXTERNAL RENDER
      EXTERNAL GCLOSE, GDELAY, GEVENT, GFLUSH, GOPEN
      INTEGER  GKEY

      INCLUDE 'event.fi'
      INCLUDE 'key.fi'
      INTEGER IDELAY, IW, IH
      PARAMETER (IDELAY=100, IW=800, IH=800)

      INTEGER IEVENT, ISTAT
      LOGICAL DONE
      DATA DONE /.FALSE./
C
C     OPEN SDL 1.2 WINDOW.
C
      CALL GOPEN(IW, IH, 'FORTRAN' // ACHAR(0), ISTAT)
      IF (ISTAT .NE. 0) STOP
C
C     RENDER THE FRACTAL.
C
      CALL RENDER(IW, IH, -1.8, -0.075, 0.04)
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
      SUBROUTINE RENDER(IW, IH, MX, MY, W)
C
C     DRAWS THE FRACTAL.
C
      EXTERNAL GCOLOR, GLAYER, GLOCK, GPIXEL, GULOCK

      INTEGER NITER, THRESH
      PARAMETER (NITER=255, THRESH=4)

      INTEGER IW, IH
      REAL    MX, MY, W

      INTEGER C, I, J, K
      REAL    CX, CY, D, PX, PY, ZX, ZY

      CALL GLAYER(0)
      CALL GLOCK()

      DO 10 I = 0, IW - 1
      DO 20 J = 0, IH - 1
      ZX = 0.0
      ZY = 0.0
      CX = MX + 2 * W * (I / (IW - 0.5))
      CY = MY + 2 * W * (J / (IH - 0.5))

      DO 30 K = 0, NITER - 1
      PX = ZX**2 - ZY**2 + CX
      PY = 2 * ABS(ZX * ZY) + CY
      ZX = PX
      ZY = PY
      D = ZX**2 + ZY**2
      IF (D .GT. THRESH) GOTO 40
   30 CONTINUE

   40 CONTINUE
      IF (K .LT. NITER) THEN
        C = MIN(255, INT(8 * ABS(K + 1 - LOG(D) / LOG(2.0))))
        CALL GCOLOR(255, C, 0)
        CALL GPIXEL(I, J)
      END IF
   20 CONTINUE
   10 CONTINUE

      CALL GULOCK()
      END
