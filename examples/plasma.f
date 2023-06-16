C     ******************************************************************
C
C     PLASMA
C
C     RENDERS ANIMATED PLASMA EFFECT:
C
C       https://rosettacode.org/wiki/Plasma_effect
C
C     DATE..: 2023-06-16
C     AUTHOR: PHILIPP ENGEL
C
C     ******************************************************************
      PROGRAM PLASMA
      EXTERNAL CREATE, RENDER
      EXTERNAL GCLOSE, GDELAY, GEVENT, GFLUSH, GOPEN
      INTEGER  GKEY

      INCLUDE 'event.fi'
      INCLUDE 'key.fi'
      INTEGER IDELAY
      PARAMETER (IDELAY=20)

      DOUBLE PRECISION SHIFT
      INTEGER IEVENT, ISTAT
      LOGICAL DONE

      DATA DONE /.FALSE./
C
C     OPEN SDL 1.2 WINDOW.
C
      CALL GOPEN(400, 400, 'FORTRAN' // ACHAR(0), ISTAT)
      IF (ISTAT .NE. 0) STOP
C
C     INITIALISE PLASMA.
C
      CALL CREATE()
      SHIFT = 0.0D0
C
C     MAIN LOOP: RENDERS PLASMA AND INCREASES HUE SHIFT.
C
   10 CONTINUE
   20 CONTINUE
      CALL GEVENT(IEVENT, ISTAT)
      IF (IEVENT .EQ. EQUIT) DONE = .TRUE.
      IF (ISTAT .EQ. 1) GOTO 20
      IF (GKEY(KESC) .EQ. 1) DONE = .TRUE.
      CALL RENDER(SHIFT)
      SHIFT = MOD(SHIFT + 0.01D0, 1.0D0)
      CALL GFLUSH()
      CALL GDELAY(IDELAY)
      IF (.NOT. DONE) GOTO 10
C
C     CLEAN UP AND QUIT.
C
      CALL GCLOSE()
      END
C     ******************************************************************
      SUBROUTINE CREATE()
C
C     INITIALISES PLASMA BUFFER.
C
      DOUBLE PRECISION BUFFER(400, 400)
      COMMON /STATE/ BUFFER

      DOUBLE PRECISION H
      INTEGER IX, IY

      DO 10 IY = 1, 400
      DO 20 IX = 1, 400
      H = SIN(IX / 16.0D0)
      H = H + SIN((IX + IY) / 16.0D0)
      H = H + SIN(SQRT(DBLE(IX**2 + IY**2)) / 8.0D0)
      H = H + 4
      H = H / 8
      H = MOD(H, 1.0D0)
      BUFFER(IX, IY) = H
   20 CONTINUE
   10 CONTINUE
      END
C     ******************************************************************
      SUBROUTINE HSV2RGB(H, S, V, IR, IG, IB)
C
C     CONVERTS HSV TO RGB.
C
      DOUBLE PRECISION H, S, V
      INTEGER          IR, IG, IB

      DOUBLE PRECISION F, HH
      INTEGER I, K, P, Q, T

      K = INT(V * 255 + 0.5)

      IF (S .EQ. 0) THEN
        IR = K
        IG = K
        IB = K
        RETURN
      END IF

      HH = (H - INT(H)) * 6
      I = INT(HH)
      F = HH - I
      P = INT(V * (1 - S) * 255 + 0.5)
      Q = INT(V * (1 - S * F) * 255 + 0.5)
      T = INT(V * (1 - S * (1 - F)) * 255 + 0.5)

      IF (I .EQ. 0) THEN
        IR = K
        IG = T
        IB = P
      ELSE IF (I .EQ. 1) THEN
        IR = Q
        IG = K
        IB = P
      ELSE IF (I .EQ. 2) THEN
        IR = P
        IG = K
        IB = T
      ELSE IF (I .EQ. 3) THEN
        IR = P
        IG = Q
        IB = K
      ELSE IF (I .EQ. 4) THEN
        IR = T
        IG = P
        IB = K
      ELSE
        IR = K
        IG = P
        IB = Q
      END IF
      END
C     ******************************************************************
      SUBROUTINE RENDER(SHIFT)
C
C     RENDERS PLASMA WITH APPLIED HUE SHIFT.
C
      EXTERNAL HSV2RGB
      EXTERNAL GCOLOR, GLOCK, GPIXEL, GULOCK

      DOUBLE PRECISION SHIFT

      DOUBLE PRECISION BUFFER(400, 400)
      COMMON /STATE/ BUFFER

      DOUBLE PRECISION H
      INTEGER IX, IY
      INTEGER IR, IG, IB

      CALL GLOCK()

      DO 10 IY = 1, 400
      DO 20 IX = 1, 400
      H = MOD(SHIFT + BUFFER(IX, IY), 1.0D0)
      CALL HSV2RGB(H, 1.0D0, 1.0D0, IR, IG, IB)
      CALL GCOLOR(IR, IG, IB)
      CALL GPIXEL(IX - 1, IY - 1)
   20 CONTINUE
   10 CONTINUE

      CALL GULOCK()
      END
