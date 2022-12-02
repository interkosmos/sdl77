C     ******************************************************************
C
C     TRACK
C
C     EXAMPLE THAT PLAYS AN AUDIO TRACK WITH SDL_MIXER. THE OGG FILE
C     MUST HAVE A SAMPLE RATE OF 22050 HZ. PRESS RETURN TO LET THE DOG
C     BARK!
C
C     DATE..: 2022-08-28
C     AUTHOR: PHILIPP ENGEL
C
C     ******************************************************************
      PROGRAM TRACK
      EXTERNAL GCLOSE, GDELAY, GEVENT, GFLUSH, GOPEN
      EXTERNAL MCHAN, MCLOSE, MHALT, MLOAD, MLOADW, MPLAY
      INTEGER  GKEY

      INCLUDE 'const.fi'
      INCLUDE 'event.fi'
      INCLUDE 'key.fi'
      INTEGER IDELAY
      PARAMETER (IDELAY=100)

      INTEGER IEVENT, ISTAT
      LOGICAL DONE, EXISTS
      DATA DONE /.FALSE./
C
C     CHECK IF AUDIO FILES EXIST.
C
      INQUIRE (EXIST=EXISTS, FILE='share/track.ogg')
      IF (.NOT. EXISTS) STOP 'Error: track.ogg not found'

      INQUIRE (EXIST=EXISTS, FILE='share/dog.wav')
      IF (.NOT. EXISTS) STOP 'Error: dog.wav not found'
C
C     OPEN WINDOW.
C
      CALL GOPEN(320, 200, 'Playing track.ogg ...' // ACHAR(0), ISTAT)
      IF (ISTAT .NE. 0) STOP 'Error: SDL failed'
C
C     LOAD MUSIC TRACK AND SOUND EFFECT.
C
      CALL MLOAD('share/track.ogg' // ACHAR(0), ISTAT)
      IF (ISTAT .EQ. 0) CALL MPLAY(-1)
      CALL MLOADW(0, 'share/dog.wav' // ACHAR(0), ISTAT)
C
C     MAIN LOOP.
C
   10 CONTINUE
   20 CONTINUE
      CALL GEVENT(IEVENT, ISTAT)
      IF (IEVENT .EQ. EQUIT) DONE = .TRUE.
      IF (ISTAT .EQ. 1) GOTO 20
      IF (GKEY(KESC) .EQ. 1) DONE = .TRUE.
      IF (GKEY(KRETRN) .EQ. 1) CALL MCHAN(0, -1, 0)

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
