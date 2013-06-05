      SUBROUTINE RECREAD_TERM
      IMPLICIT NONE

*     FORMAL_PARAMETERS:

**    common block
      INCLUDE 'recread.inc'

      SAVE

      IF (.NOT.FILE_CLOSED) THEN
         CLOSE (L_UNIT)
         FILE_CLOSED = .TRUE.
      END IF

      RETURN
      END
