      SUBROUTINE RECREAD (STBUF,RECLEN,STBLEN,EOF,IWAR)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      CHARACTER*(*) STBUF
      LOGICAL EOF
      INTEGER RECLEN,STBLEN,IWAR

**    Local variables
      INCLUDE 'recread.inc'
      INTEGER F_BUF_LEN,IOS

      SAVE

      IF (.NOT.INIT) CALL FATALERR ('RECREAD','system not initialized')
      IWAR = 0

      READ (L_UNIT,'(A)',IOSTAT=IOS) F_BUF
      EOF = IOS.NE.0

      IF (.NOT.EOF) THEN
*        Determine significant length
         DO F_BUF_LEN=F_BUF_DEC_LEN,1,-1
            IF (F_BUF(F_BUF_LEN:F_BUF_LEN).NE.' ') GOTO 20
         END DO

         F_BUF_LEN = 0

20       CONTINUE

         IF (F_BUF_LEN.GT.0.AND.F_BUF_LEN.LE.RECLEN) THEN
*           Input record fits on user record
            STBUF  = F_BUF(1:F_BUF_LEN)
            STBLEN = F_BUF_LEN
         ELSE IF (F_BUF_LEN.EQ.0) THEN
*           Empty input record
            STBUF  = ' '
            STBLEN = 0
         ELSE
*           Input record too long
            STBUF  = F_BUF(1:RECLEN)
            IWAR   = 1
            STBLEN = RECLEN
         END IF
      ELSE
         CLOSE (L_UNIT)
         FILE_CLOSED = .TRUE.
         F_BUF_LEN = 0
         STBUF     = ' '
         STBLEN    = 0
         INIT      = .FALSE.
      END IF

      RETURN
      END
