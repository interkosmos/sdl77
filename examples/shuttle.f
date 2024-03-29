C     ******************************************************************
C
C     3-D SPACE SHUTTLE
C
C     PORT OF THE GW-BASIC PROGRAM "SHUTTLE.BAS" THAT DISPLAYS THE
C     WIREFRAME MODEL OF A SPACE SHUTTLE.
C
C     DATE..: 2022-08-28
C     AUTHOR: PHILIPP ENGEL
C
C     ******************************************************************
      PROGRAM SHUTTLE
      EXTERNAL  GCLOSE, GCOLOR, GCUR, GDELAY, GEVENT, GFILL, GFLUSH
      EXTERNAL  GLINE, GOPEN
      INTEGER   GKEY
      INTEGER*8 GTIME

      INCLUDE 'event.fi'
      INCLUDE 'key.fi'
      INTEGER IDELAY, IW, IH, NEDGES, NVERTS
      REAL    ALPHA, D, F, PI2
      PARAMETER (IDELAY=20, IW=640, IH=480)
      PARAMETER (NEDGES=259, NVERTS=124)
      PARAMETER (ALPHA=0.01, D=120.0, F=0.2, PI2=ACOS(-1.0)*2)

      INTEGER I, IEVENT, ISTAT, IX1, IX2, IY1, IY2
      LOGICAL DONE
      REAL    B, H, P
      REAL    CH, SH, CP, SP, CB, SB
      REAL    AM, BM, CM, DM, EM, FM, GM, HM, IM
      REAL    X, Y, Z, XV, YV, ZV, X3, Y3, Z3
      REAL    U1, V1, U2, V2

      INTEGER EDGES(NEDGES)
      REAL    VERTS(NVERTS, 3)
C
C     SCALE THE VERTICES.
C
      DO 10 I = 1, NVERTS
      VERTS(I, 1) = VERTS(I, 1) * F
      VERTS(I, 2) = VERTS(I, 2) * F
      VERTS(I, 3) = VERTS(I, 3) * F
   10 CONTINUE
C
C     OPEN SDL 1.2 WINDOW.
C
      CALL GOPEN(IW, IH, 'FORTRAN SHUTTLE' // ACHAR(0), ISTAT)
      CALL GCUR(0)
C
C     SET RANDOM ORIENTATION.
C
      CALL SRAND(ABS(INT(GTIME())))

      B = RAND(0) * PI2
      H = RAND(0) * PI2
      P = RAND(0) * PI2

      U2 = 0.0
      V2 = 0.0
C
C     RENDER LOOP.
C
      DONE = .FALSE.

   20 CONTINUE
      CALL GEVENT(IEVENT, ISTAT)
      IF (IEVENT .EQ. EQUIT) DONE = .TRUE.

      CALL GCOLOR(0, 0, 0)
      CALL GFILL()
      CALL GCOLOR(255, 255, 0)
C
C     ROTATE VERTICES.
C
      P = MOD(P + ALPHA, PI2)
      B = MOD(B + ALPHA, PI2)

      CH = COS(H)
      SH = SIN(H)
      CP = COS(P)
      SP = SIN(P)
      CB = COS(B)
      SB = SIN(B)

      AM = CB * CH - SH * SP * SB
      BM = -CB * SH - SP * CH * SB
      CM = CP * SB
      DM = SH * CP
      EM = CP * CH
      FM = SP
      GM = -CH * SB - SH * SP * CB
      HM = SH * SB - SP * CH * CB
      IM = CP * CB

      XV = -D * CP * SH
      YV = -D * CP * CH
      ZV = -D * SP
C
C     DRAW EDGES.
C
      DO 30 I = 1, NEDGES
      X = VERTS(ABS(EDGES(I)), 1)
      Y = VERTS(ABS(EDGES(I)), 2)
      Z = VERTS(ABS(EDGES(I)), 3)

      X = X - XV
      Y = Y - YV
      Z = Z - ZV

      X3 = AM * X + BM * Y + CM * Z
      Y3 = DM * X + EM * Y + FM * Z
      Z3 = GM * X + HM * Y + IM * Z

      U1 = 135 + 13.5 * D * X3 / Y3
      V1 =  80 - 11.5 * D * Z3 / Y3

      IF (EDGES(I) .GT. 0) THEN
        IX1 = INT(U2 * 2 + 75)
        IY1 = INT(V2 + 100)
        IX2 = INT(U1 * 2 + 75)
        IY2 = INT(V1 + 100)
        CALL GLINE(IX1, IY1, IX2, IY2)
      END IF

      U2 = U1
      V2 = V1
   30 CONTINUE
C
C     CHECK FOR KEYBOARD INPUT AND COPY LAYER TO SCREEN.
C
      IF (GKEY(KESC) .EQ. 1) DONE = .TRUE.
      CALL GFLUSH()
      CALL GDELAY(IDELAY)
      IF (.NOT. DONE) GOTO 20
C
C     CLEAN UP AND QUIT.
C
      CALL GCUR(1)
      CALL GCLOSE()

      DATA VERTS /0.,1.5,2.2,1.7,0.,-1.7,-2.2,-1.5,0.,2.8,4.,3.,0.,-3.,
     &  -4.,-2.8,0.,4.6,5.8,4.,0.,-4.,-5.8,-4.6,0.,4.5,5.8,4.,0.,-4.,
     &  -5.8,-4.5,0.,3.5,7.8,8.,0.,-8.,-7.8,-3.5,0.,3.8,8.,8.,0.,-8.,
     &  -8.,-3.8,0.,4.7,8.,8.,0.,-8.,-8.,-4.7,0.,4.7,8.,8.,0.,-8.,-8.,
     &  -4.7,0.,4.7,8.,8.,0.,-8.,-8.,-4.7,0.,4.7,8.,8.,0.,-8.,-8.,-4.7,
     &  0.,4.7,8.,8.,0.,-8.,-8.,-4.7,0.,2.,8.8,9.,0.,-9.,-8.8,-2.,0.,2.,
     &  9.2,10.,0.,-10.,-9.2,-2.,8.7,15.,35.,35.,-8.7,-15.,-35.,-35.,0.,
     &  0.,0.,0.,6.,6.,11.,11.,-6.,-6.,-11.,-11.,-2.2,-2.6,-4.6,-6.5,
     &  -6.7,-6.5,-4.6,-2.6,-0.8,-1.5,-4.5,-7.2,-8.,-7.2,-4.5,-1.5,1.7,
     &  0.,-4.4,-8.2,-9.,-8.2,-4.4,0.,4.,1.,-4.6,-9.,-9.5,-9.,-4.6,1.,
     &  8.,7.,2.,-7.,-9.8,-7.,2.,7.,8.,7.5,3.,-8.,-9.8,-8.,3.,7.5,8.,7.,
     &  4.,-8.7,-10.,-8.7,4.,7.,8.,7.,4.,-8.7,-10.,-8.7,4.,7.,8.,7.,4.,
     &  -8.7,-10.,-8.7,4.,7.,8.,7.,4.,-8.7,-10.,-8.7,4.,7.,8.,7.,4.,
     &  -8.7,-10.,-8.7,4.,7.,9.,8.5,1.5,-10.,-10.8,-10.,1.5,8.5,9.5,9.3,
     &  1.5,-10.,-10.2,-10.,1.5,9.3,-8.7,-8.7,-10.,-10.,-8.7,-8.7,-10.,
     &  -10.,13.,33.,33.,14.,11.,11.,5.,5.,11.,11.,5.,5.,46.,46.,46.,
     &  46.,46.,46.,46.,46.,43.,43.,43.,43.,43.,43.,43.,43.,38.,38.,38.,
     &  38.,38.,38.,38.,38.,32.5,32.5,32.5,32.5,32.5,32.5,32.5,32.5,
     &  26.3,26.3,26.3,26.3,26.3,26.3,26.3,26.3,21.5,21.5,21.5,21.5,
     &  21.5,21.5,21.5,21.5,14.,14.,14.,14.,14.,14.,14.,14.,4.,4.,4.,4.,
     &  4.,4.,4.,4.,-12.,-12.,-12.,-12.,-12.,-12.,-12.,-12.,-27.3,-27.3,
     &  -27.3,-27.3,-27.3,-27.3,-27.3,-27.3,-35.6,-35.6,-35.6,-35.6,
     &  -35.6,-35.6,-35.6,-35.6,-43.,-43.,-43.,-43.,-43.,-43.,-43.,-43.,
     &  -48.,-48.,-48.,-48.,-48.,-48.,-48.,-48.,21.,-16.,-36.,-40.,21.,
     &  -16.,-36.,-40.,-37.,-60.,-69.,-60.,-43.,-48.,-43.,-48.,-43.,
     &  -48.,-43.,-48./
      DATA EDGES /-1,2,3,4,5,6,7,8,1,-9,10,11,12,13,14,15,16,9,-17,18,
     &  19,20,21,22,23,24,17,-25,26,27,28,29,30,31,32,25,-33,34,35,36,
     &  37,38,39,40,33,-41,42,43,44,45,46,47,48,41,-49,50,51,52,53,54,
     &  55,56,49,-57,58,59,60,61,62,63,64,57,-65,66,67,68,69,70,71,72,
     &  65,-73,74,75,76,77,78,79,80,73,-81,82,83,84,85,86,87,88,81,-89,
     &  90,91,92,93,94,95,96,89,-97,98,99,100,101,102,103,104,97,-1,9,
     &  17,25,33,41,49,57,65,73,81,89,97,-2,10,18,26,34,42,50,58,66,74,
     &  82,90,98,-3,11,19,27,35,43,51,59,67,75,83,91,99,-4,12,20,28,36,
     &  44,52,60,68,76,84,92,100,-5,13,21,29,37,45,53,61,69,77,85,93,
     &  101,-6,14,22,30,38,46,54,62,70,78,86,94,102,-7,15,23,31,39,47,
     &  55,63,71,79,87,95,103,-8,16,24,32,40,48,56,64,72,80,88,96,104,
     &  -44,105,106,107,108,92,-46,109,110,111,112,94,-81,113,114,115,
     &  116,89,-82,117,118,-83,119,120,-87,121,122,-88,123,124,-117,119,
     &  -121,123,-118,120,-122,124/
      END
