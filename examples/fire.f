C     ******************************************************************
C
C     FIRE
C
C     SDL77 DEMO PROGRAM THAT RENDERS THE DOOM FIRE EFFECT TO SCREEN.
C
C     DATE..: 2022-08-28
C     AUTHOR: PHILIPP ENGEL
C
C     ******************************************************************
      PROGRAM DEMO
C
C     EXTERNAL PROCEDURES.
C
      EXTERNAL FBURN, FIGNIT
      EXTERNAL GCLOSE, GCUR, GDELAY, GEVENT, GFLUSH, GOPEN
      INTEGER  GKEY, GTICKS, GTIME
C
C     PARAMETERS.
C
      INCLUDE 'const.fi'
      INTEGER MAXFPS
      REAL    FT
      PARAMETER (MAXFPS=30, FT=1.0/MAXFPS*1000)
C
C     VARIABLES.
C
      INTEGER IDELAY, IDT, IEVENT, ISTAT, IT
      LOGICAL DONE
      DATA DONE /.FALSE./
C
C     OPEN SDL 1.2 WINDOW.
C
      CALL SRAND(GTIME())
      CALL GOPEN(IW, IH, 'FORTRAN' // ACHAR(0), ISTAT)
      IF (ISTAT .NE. 0) STOP
      CALL INFO()
      CALL GCUR(0)
C
C     INITIALISE COLOUR PALETTE AND IGNITE FIRE.
C
      CALL FIGNIT()
C
C     MAIN LOOP.
C
   30 CONTINUE
      IT = GTICKS()
   40 CONTINUE
      CALL GEVENT(IEVENT, ISTAT)
      IF (IEVENT .EQ. IEQUIT) DONE = .TRUE.
      IF (ISTAT .EQ. 1) GOTO 40

      IF (GKEY(IESC) .EQ. 1) DONE = .TRUE.
      CALL FBURN(320)
      CALL GFLUSH()

      IDT = GTICKS() - IT
      IDELAY = MAX(0, INT(FT - IDT))
      IF (IDELAY .GT. 0) CALL GDELAY(IDELAY)
      IF (.NOT. DONE) GOTO 30
C
C     CLEAN UP AND QUIT.
C
      CALL GCUR(1)
      CALL GCLOSE()
      END
C     ******************************************************************
      SUBROUTINE INFO()
C
C     PRINTS SDL 1.2 INFORMATION TO STANDARD OUTPUT.
C
      EXTERNAL GVIDEO

      INTEGER   IHW, IWM
      INTEGER*8 IVM

      CALL GVIDEO(IHW, IWM, IVM)
      PRINT 100, IHW, IWM
      IF (IHW .EQ. 1) PRINT 200, IVM

  100 FORMAT ('Hardware Acceleration: ',I1,/,
     &'Window Manager.......: ',I1)
  200 FORMAT ('Video Memory.........: ',I1,' KiB')
      END
C     ******************************************************************
      SUBROUTINE FBURN(IY)
C
C     LETS THE FIRE BURN. RENDERS TO Y POSITION IY OF SURFACE.
C
      EXTERNAL GCPPAL, GLAYER, GLOCK, GULOCK

      INCLUDE 'const.fi'
      INTEGER IFW, IFH, N
      PARAMETER (IFW=IW/2, IFH=140, N=IFW*IFH)

      INTEGER IY
      INTEGER IFIREY(N), I, J, K, L, M, P
      REAL    R
      COMMON /FIRE/ IFIREY

      DO 10 J = 1, IFH - 1
      DO 20 I = 0, IFW - 1
      K = (J * IFW) + I + 1
      P = IFIREY(K)

      IF (P .EQ. 0) THEN
        IFIREY(K - IFW) = 0
      ELSE
        R = RAND()
        L = IAND(INT(R * 3), 3)
        M = 1 + MOD(K - IFW - L, N)
        IFIREY(M) = P - IAND(L, 1)
      END IF
   20 CONTINUE
   10 CONTINUE

      CALL GLAYER(0)
      CALL GLOCK()
      DO 30 J = 0, IFH - 1
      DO 40 I = 0, IFW - 1
      K = (J * IFW) + I + 1
C      IF (IFIREY(K) .EQ. 0) CYCLE
      CALL GCPPAL(I * 2,     IY + J * 2, IFIREY(K))
      CALL GCPPAL(I * 2 + 1, IY + J * 2, IFIREY(K))
   40 CONTINUE
      DO 50 I = 0, IFW - 1
      K = (J * IFW) + I + 1
C      IF (IFIREY(K) .EQ. 0) CYCLE
      CALL GCPPAL(I * 2,     IY + J * 2 + 1, IFIREY(K))
      CALL GCPPAL(I * 2 + 1, IY + J * 2 + 1, IFIREY(K))
   50 CONTINUE
   30 CONTINUE
      CALL GULOCK()
      END
C     ******************************************************************
      SUBROUTINE FIGNIT()
C
C     IGNITES THE FIRE.
C
      EXTERNAL GPAL, GSETP

      INCLUDE 'const.fi'
      INTEGER IFW, IFH, N, NCOL
      PARAMETER (IFW=IW/2, IFH=140, N=IFW*IFH, NCOL=37)

      INTEGER IFIREY(N), IPALY(NCOL*3)
      INTEGER I, IR, IG, IB
      COMMON /FIRE/   IFIREY

      CALL GPAL(NCOL)

      DO 10 I = 0, (NCOL * 3) - 1, 3
      IR = IPALY(I + 1)
      IG = IPALY(I + 2)
      IB = IPALY(I + 3)
      CALL GSETP(I / 3, IR, IG, IB)
   10 CONTINUE

      DO 20 I = 1, IFW
      IFIREY((IFH - 1) * IFW + I) = 36
   20 CONTINUE

      DATA IFIREY /N*0/
      DATA IPALY  /0,0,0,31,7,7,47,15,7,71,15,7,87,23,7,103,31,7,119,31,
     &7,143,39,7,159,47,7,175,63,7,191,71,7,199,71,7,223,79,7,223,87,7,
     &223,87,7,215,95,7,215,95,7,215,103,15,207,111,15,207,119,15,207,
     &127,15,207,135,23,199,135,23,199,143,23,199,151,31,191,159,31,191,
     &159,31,191,167,39,191,167,39,191,175,47,183,175,47,183,183,47,183,
     &183,55,207,207,111,223,223,159,239,239,199,255,255,255/
      END
