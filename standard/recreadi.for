      SUBROUTINE RECREAD_INIT (UNIT,INPUT_FILE)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      CHARACTER*(*) INPUT_FILE
      INTEGER UNIT

**    Include files
      INCLUDE 'recread.inc'

      SAVE

      L_UNIT = UNIT
      CALL FOPENS (L_UNIT,INPUT_FILE,'RDO',' ')

      FILE_CLOSED = .FALSE.
      INIT        = .TRUE.

      RETURN
      END

      BLOCK DATA RECREAD_DATA
      IMPLICIT NONE
      INCLUDE 'recread.inc'
      SAVE
      DATA INIT /.FALSE./
      END
