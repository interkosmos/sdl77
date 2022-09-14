C     ******************************************************************
C
C     TRACK
C
C     EXAMPLE THAT PLAYS AN AUDIO TRACK WITH SDL_MIXER. THE OGG FILE
C     MUST HAVE A SAMPLE RATE OF 22050 HZ.
C
C     DATE..: 2022-08-28
C     AUTHOR: PHILIPP ENGEL
C
C     ******************************************************************
      PROGRAM TRACK
      EXTERNAL GCLOSE, GDELAY, GEVENT, GFLUSH, GOPEN
      EXTERNAL MCLOSE, MHALT, MOPEN, MPLAY
      INTEGER  GKEY

      INCLUDE 'const.fi'
      INTEGER IDELAY
      PARAMETER (IDELAY=100)

      INTEGER IEVENT, ISTAT
      LOGICAL DONE, EXISTS
      DATA DONE /.FALSE./
C
C     CHECK OGG FILE.
C
      INQUIRE (EXIST=EXISTS, FILE='share/track.ogg')
      IF (.NOT. EXISTS) STOP 'ERROR: FILE NOT FOUND'
C
C     OPEN WINDOW.
C
      CALL GOPEN(320, 200, 'Playing track.ogg ...' // ACHAR(0), ISTAT)
      IF (ISTAT .NE. 0) STOP 'ERROR: SDL FAILED'
C
C     PLAY TRACK INDEFINITELY.
C
      CALL MOPEN('share/track.ogg' // ACHAR(0))
      CALL MPLAY(-1)
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
C     QUIT.
C
      CALL MHALT()
      CALL MCLOSE()
      CALL GCLOSE()
      END
