C     ******************************************************************
C
C     FIZZLE
C
C     SDL77 DEMO PROGRAM THAT DEMONSTRATES A FIZZLE-FADE EFFECT BASED
C     ON FISHER-YATES SHUFFLE. REQUIRES THE PRNG PROCEDURES SRAND() AND
C     RAND().
C
C     DATE..: 2022-08-28
C     AUTHOR: PHILIPP ENGEL
C
C     ******************************************************************
      PROGRAM DEMO
      EXTERNAL  GCLOSE, GCUR, GDELAY, GEVENT, GFILL, GFLUSH, GOPEN
      EXTERNAL  ZINIT
      INTEGER   GKEY
      INTEGER*8 GTIME
      LOGICAL   ZNEXT

      INCLUDE 'const.fi'
      INCLUDE 'event.fi'
      INCLUDE 'key.fi'
      INTEGER IDELAY
      PARAMETER (IDELAY=50)

      INTEGER IEVENT, ISTAT
      LOGICAL DONE, NEXT
      DATA DONE, NEXT /.FALSE.,.TRUE./

      CALL SRAND(INT(GTIME() - (2**30 - 1)))
C
C     OPEN WINDOW, HIDE CURSOR, INITIALISE FIZZLE FADE.
C
      CALL GOPEN(IW, IH, 'FORTRAN' // ACHAR(0), ISTAT)
      IF (ISTAT .NE. 0) STOP
      CALL GCUR(0)
      CALL ZINIT(255, 0, 0)
C
C     MAIN LOOP: POLLS EVENTS, BLITS SURFACE, FLIPS BUFFER TO SCREEN.
C
   10 CONTINUE
   20 CONTINUE
      CALL GEVENT(IEVENT, ISTAT)
      IF (IEVENT .EQ. EQUIT) DONE = .TRUE.
      IF (ISTAT .EQ. 1) GOTO 20
      IF (GKEY(KESC) .EQ. 1) DONE = .TRUE.

      IF (NEXT) THEN
        NEXT = ZNEXT()
      ELSE
        CALL GCOLOR(0, 0, 0)
        CALL GFILL()
        CALL ZRESET()
        NEXT = .TRUE.
      END IF

      CALL GFLUSH()
      CALL GDELAY(IDELAY)
      IF (.NOT. DONE) GOTO 10
C
C     CLEAN UP AND QUIT.
C
      CALL GCUR(1)
      CALL GCLOSE()
      END
C     ******************************************************************
      LOGICAL FUNCTION ZNEXT()
C
C     FIZZLES THE SCREEN NDOTS TIMES PER CALL. RETURNS FALSE IF
C     FINISHED, ELSE TRUE.
C
      EXTERNAL GCOLOR, GLOCK, GPIXEL, GULOCK
      INCLUDE 'const.fi'
      INTEGER N, NDOTS
      PARAMETER (N=IW*IH, NDOTS=N/60)

      INTEGER IZR, IZG, IZB, INEXT, ISHUFY(N)
      INTEGER IX, IY

      COMMON /FIZZLE/ IZR, IZG, IZB, INEXT, ISHUFY

      ZNEXT = .FALSE.
      IF (INEXT .GT. N) RETURN

      CALL GCOLOR(IZR, IZG, IZB)
      CALL GLOCK()

      DO 10 INEXT = INEXT, MIN(N, INEXT + NDOTS)
      IX = MOD(ISHUFY(INEXT), IW)
      IY = ISHUFY(INEXT) / IW
      CALL GPIXEL(IX, IY)
   10 CONTINUE

      CALL GULOCK()
      IF (INEXT .LE. N) ZNEXT = .TRUE.
      END
C     ******************************************************************
      SUBROUTINE ZINIT(IR, IG, IB)
C
C     INITIALISES THE FIZZLE EFFECT BY ADDING ALL SCREEN COORDINATES TO
C     ARRAY ISHUFY AND ITERATING A FISHER-YATES-SHUFFLE OVER IT.
C
      INCLUDE 'const.fi'
      INTEGER IR, IG, IB
      INTEGER I, J, K, N
      INTEGER IZR, IZG, IZB, INEXT, ISHUFY(IW*IH)
      REAL    R

      COMMON /FIZZLE/ IZR, IZG, IZB, INEXT, ISHUFY
      DATA INEXT /1/

      IZR = IR
      IZG = IG
      IZB = IB

      N = IW * IH

      DO 10 I = 1, N
      ISHUFY(I) = I - 1
   10 CONTINUE

      DO 20 I = N, 2, -1
      R = RAND()
      J = INT(R * I) + 1
      K = ISHUFY(J)
      ISHUFY(J) = ISHUFY(I)
      ISHUFY(I) = K
   20 CONTINUE
      END
C     ******************************************************************
      SUBROUTINE ZRESET()
C
C     RESETS THE FIZZLE INDEX.
C
      INCLUDE 'const.fi'
      INTEGER IZR, IZG, IZB, INEXT, ISHUFY(IW*IH)
      COMMON /FIZZLE/ IZR, IZG, IZB, INEXT, ISHUFY
      INEXT = 1
      END
