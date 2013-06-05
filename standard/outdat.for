      SUBROUTINE OUTDAT (ITASK, IUNIT, RN, R)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      INTEGER ITASK, IUNIT
      REAL R
      CHARACTER*(*) RN

**    local variables
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*     A D J U S T A B L E   P A R A M E T E R S                       *
*     =========================================                       *
*     IMNC1  = maximum number of columns of dependent variables in an *
*              output block, if increased, also increase LINE*x, x    *
*              should be at least 14+IMNC1*13,                        *
*     NAMES_MXN = maximum number of names of dependent variables,     *
*                 can be increased without problems,                  *
*     VARL_M = maximum length of a variable, remainder is removed     *
*     Warning: do not change the maximum length of names of variables,*
*              currently set to 11 !!                                 *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

*     AVN    - Array of variable names
*     IAVNL  - Array of lengths of AVN
*     AVV    - Array with values of AVN
*     ASELN  - Array with selected variable names
*     SEL    - Array with flags whether variables from AVN should be
*              printed
*     FND    - Array with counts how many times a variable is seen at
*              itask=2
*     MGIVEN - Array of flags whether messages about repeated input
*              of a variable is given
*     BLK    - Assigned block number, 0 if not assigned
*     COL_WIDTH_MNN
*            - maximum width of a Y column, excluding spaces
*     SEQ    - SEQ(1) = element of AVN that is first in
*                       current block,
*              SEQ(2) = element of AVN that is second in
*                       current block etc.
*     EXTRA_SP - Extra width of columns
*     EXTRA_SPX- Extra width for X columns
*     NAMES_IB_MXN_4 - Maximum number of names in block in itask=4
*                      (determined by paper width)
*     NAMES_IB_MXN_5 - Maximum number of names in block in itask=5
*     NAMES_IB_MXN_7 - Maximum number of names in block in itask=7
*     NAMES_IB_MXN_8 - Maximum number of names in block in itask=8
*     NAMES_IB_MXN_9 - Maximum number of names in block in itask=9

*     SEQ2     - Array with numbers that link to selected arrays

*     Maximum length of a variable name is 36 characters (31 for name,
*     5 for possible index e.g. '(134)')

      INTEGER NAMES_MXN, VARL_M
      INTEGER NAMES_IB_MXN_4,NAMES_IB_MXN_5
      INTEGER NAMES_IB_MXN_7,NAMES_IB_MXN_8,NAMES_IB_MXN_9
      PARAMETER (NAMES_IB_MXN_4=8)
      PARAMETER (NAMES_IB_MXN_5=100)
      PARAMETER (NAMES_IB_MXN_7=8)
      PARAMETER (NAMES_IB_MXN_8=NAMES_IB_MXN_5)
      PARAMETER (NAMES_IB_MXN_9=NAMES_IB_MXN_5)
      PARAMETER (NAMES_MXN=500,VARL_M=36)
      CHARACTER*(VARL_M) AVN(NAMES_MXN)
      INTEGER            IAVNL(NAMES_MXN)
      CHARACTER*(VARL_M) ASELN(NAMES_MXN)
      INTEGER            BLK(NAMES_MXN)
      LOGICAL            MGIVEN(NAMES_MXN)
      INTEGER            SEQ2(NAMES_MXN)
      INTEGER            FND(NAMES_MXN)
      CHARACTER*(VARL_M) LXN, LN
      REAL               AVV(NAMES_IB_MXN_5)
      LOGICAL            FNDA(NAMES_IB_MXN_5)
      INTEGER            SEQ(NAMES_IB_MXN_5)
      INTEGER            EXTRA_SP(NAMES_IB_MXN_5),EXTRA_SPX

      INTEGER COL_WIDTH_MNN,LINE_LEN,CENTRE
      INTEGER LINE_LEN_4
      INTEGER LINE_LEN_5
      INTEGER LINE_LEN_7
      INTEGER LINE_LEN_8
      INTEGER LINE_LEN_9

      PARAMETER (COL_WIDTH_MNN=12,CENTRE=8)
      PARAMETER (LINE_LEN_4=
     &    COL_WIDTH_MNN+2+NAMES_IB_MXN_4*(COL_WIDTH_MNN+1))
      PARAMETER (LINE_LEN_5=
     &    COL_WIDTH_MNN+2+NAMES_IB_MXN_5*(COL_WIDTH_MNN+1))
      PARAMETER (LINE_LEN_7=
     &    COL_WIDTH_MNN+2+NAMES_IB_MXN_7*(COL_WIDTH_MNN+1))
      PARAMETER (LINE_LEN_8=LINE_LEN_5)
      PARAMETER (LINE_LEN_9=LINE_LEN_5)

*     declaration of longest line, others should fit into this one
      CHARACTER*(LINE_LEN_5) LINE

*     number of different variable names that have been found in a
*     particular run
      INTEGER IFND

*     IFND2 is the TRUE number of variables to be printed in a
*     particular block
      INTEGER IFND2

*     flag whether block is full with columns
      LOGICAL FULL_BLOCK

*     Length of LINE
      INTEGER LINE_L

*     Flag whether extra spaces between columns are necessary
      LOGICAL EXTRA_F

**    other local parameters
      INTEGER ILU1, ILU2, ICHECK, IOS, INSEL, ISEL, ISAVE
      INTEGER IRUN1, IRUN2, IRUN3, IRUN4, IRUN5
      INTEGER ITOLD, ILTASK, INAME, IBLOK, ILXN, IR
      INTEGER ISREC, IREC, IEREC, IB, I1, I2, I3, I4, I5, I6, IFINDC
      REAL LV, LVO
      CHARACTER CHR*1, RUNTYP*1, RUNDUM*1, TEXT(4:9)*18, COMMCHR*1
      LOGICAL OPEND, OK, YFND, RECOVR, SELECTED, FIRST8, FIRST9
      INTEGER UNLOG
      LOGICAL TOSCR, TOLOG
      CHARACTER*80 SPACE

*     uncomment for mac absoft fortran compilation
*      CHARACTER EOFCHR*1

      SAVE

*     uncomment for mac absoft fortran compilation
*      DATA EOFCHR /'Z'/

      DATA TEXT /'Table output','Spreadsheet output',
     &           '2 column output','ICASA output',
     &           'End of run output','Greenery output'/

      DATA ITOLD /-1/, IRUN1 /0/, IRUN2 /0/, INSEL /0/, RECOVR /.FALSE./
      DATA FIRST8 /.TRUE./
      DATA FIRST9 /.TRUE./

      IF (ITASK.EQ.1) THEN

*        unit number and status check:
*        =============================
*        unit number must be > 0 at first call, may be zero or equal
*        to value at first call

         IF (ITOLD.EQ.-1) THEN

*           desired message output
            CALL MESSINQ (TOSCR, TOLOG, UNLOG)

*           OUTDAT writes messages to its formatted output file
*           and no additional logfile output is generated.
*           So messages to screen require TOSCR to tbe true
*           TOLOG and UNLOG are ignored.

            IF (IUNIT.EQ.0) CALL FATALERR
     &         ('OUTDAT','no unit number supplied')
            ILU2 = IUNIT

         ELSE

            IF (IUNIT.NE.0.AND.IUNIT.NE.ILU2) CALL FATALERR
     &         ('OUTDAT','change of unit number not allowed')

            IF (ITOLD.EQ.1) THEN

*              repeated initialization is taking place

               IF (TOSCR) WRITE (*,'(2A)') ' WARNING from OUTDAT:',
     &           ' ignoring repeated initialization'
               WRITE (ILU2,'(2A)') ' WARNING from OUTDAT:',
     &           ' ignoring repeated initialization'
               RETURN

            ELSE IF (ITOLD.EQ.2) THEN

               CONTINUE

            ELSE IF (ITOLD.EQ.3) THEN

*              during a previous call, one or more variables were
*              selected, this selection is discarded after
*              initialization

               IF (TOSCR) WRITE (*,'(2A)') ' WARNING from OUTDAT:',
     &           ' selected variables discarded'
               WRITE (ILU2,'(2A)') ' WARNING from OUTDAT:',
     &           ' selected variables discarded'

            ELSE IF (ITOLD.GE.4) THEN

               IF (RECOVR) CALL FATALERR ('OUTDAT',
     &            'normal OUTDAT call impossible after recovery')

            END IF
         END IF

*        make unit number for temporary file local and make unit number
*        available also for OUTPLT
         ILU1 = ILU2+1
         CALL AMBUSY (1,'OUTDAT',ILU2)

*        open file check:
*        see if units ILU2 and ILU1 are open. if not open here

         INQUIRE (UNIT=ILU2, OPENED=OPEND)
         IF (.NOT.OPEND) CALL FOPENG (ILU2,'RES.DAT','NEW','SF',0,'DEL')

*        see if unit ILU1 is open, if open use it, if not, open using
*        default output file name
         INQUIRE (UNIT=ILU1, OPENED=OPEND)
         IF (.NOT.OPEND) THEN
*           for normal compilers
            CALL FOPENG (ILU1,'RES.BIN','NEW','UD',48,'DEL')

*           reset number of runs in RES.BIN file, first record to
*           write to and start record of set
            IRUN4 = 0
            IREC  = 1
            ISREC = 0
         ELSE
*           temporary file is open
            IF (ITOLD.EQ.-1) CALL FATALERR ('OUTDAT',
     &      'temporary file may not be opened outside OUTDAT')
         END IF

*        initialize routine that writes comment lines to output file
         CALL OUTCOM ('<INIT$$$>')

*        reset arrays with names
         DO I1=1,NAMES_MXN
            AVN(I1)    = ' '
            FND(I1)    = 0
            MGIVEN(I1) = .FALSE.
         END DO

*        find out if initialization if generated by reruns
         CALL AMBUSY (2,'RDFROM',IRUN2)
         IF (IRUN2.EQ.0) THEN
*           run number not obtained from RDFROM or first run
            IRUN3  = IRUN1
            RUNTYP = 'N'
         ELSE
*           run number obtained from RDFROM
            IRUN3  = IRUN2
            RUNTYP = 'R'
         END IF

*        increase run number and number of runs in file
         IRUN1 = IRUN1+1
         IRUN4 = IRUN4+1

*        write total number of runs in RES.BIN file to RES.BIN file
         WRITE (ILU1,REC=1)
     &     '-',IRUN4,'....................................',0.

*        update pointer record from previous set only if it is not
*        the first initialization to the same RES.BIN file

         IF (IRUN4.GE.2) WRITE (ILU1,REC=ISREC)
     &      ' ',IREC,'....................................',0.
         IREC  = IREC+1
         ISREC = IREC

*        check on length of name
         IF (LEN (RN).LE.VARL_M) THEN
*           length of name is ok
            LXN = RN
         ELSE
*           length of name is not ok
            I1 = LEN_TRIM (RN)
            IF (TOSCR) WRITE (*,'(2A,/,2A)')
     &        ' WARNING from OUTDAT: variable ',RN(1:I1),
     &        ' is too long for OUTDAT, it is truncated to: ',
     &          RN(1:VARL_M)
            WRITE (ILU2,'(2A,/,2A)')
     &        ' WARNING from OUTDAT: variable ',RN(1:I1),
     &        ' is too long for OUTDAT, it is truncated to: ',
     &          RN(1:VARL_M)
            LXN = RN(1:VARL_M)
         END IF

         CALL UPPERC (LXN)
         ILXN  = LEN_TRIM (LXN)

         WRITE (ILU1, REC=IREC)
     &     ' ',-99,'....................................',0.
         IREC  = IREC+1
         WRITE (ILU1, REC=IREC) RUNTYP,IRUN3,LXN,R

*        uncomment for mac absoft fortran compilation
*         WRITE (ILU1, REC=IREC+1)
*     &     EOFCHR,0,'....................................',0.

         IFND  = 0
         INSEL = 0
         ISAVE = 0
         SPACE = ' '

      ELSE IF (ITASK.EQ.2) THEN

*        dump variable to file

*        check status first
         IF (ITOLD.GE.3) CALL FATALERR
     &          ('OUTDAT','Initialization not done')

*        check on length of name
         IF (LEN (RN).LE.VARL_M) THEN
*           length of name is ok
            LN = RN
         ELSE
*           length of name is not ok
            I1 = LEN_TRIM (RN)
            IF (TOSCR) WRITE (*,'(2A,/,2A)')
     &        ' WARNING from OUTDAT: variable ',RN(1:I1),
     &        ' is too long for OUTDAT, it is truncated to: ',
     &          RN(1:VARL_M)
            WRITE (ILU2,'(2A,/,2A)')
     &        ' WARNING from OUTDAT: variable ',RN(1:I1),
     &        ' is too long for OUTDAT, it is truncated to: ',
     &          RN(1:VARL_M)
            LN = RN(1:VARL_M)
         END IF

*        values and names are written to file

         CALL UPPERC (LN)

         OK    = .TRUE.
         INAME = 0

         IF (LN.EQ.LXN) THEN
            DO I1=1,IFND
               FND(I1) = 0
            END DO
            ISAVE = 0
         ELSE
*           variable is not the independent variable, look in list

*           increase new try pointer value, if at end of list reset
            I1 = ISAVE+1
            IF (I1.GT.IFND) I1 = 1

            IF (I1.GT.ISAVE) THEN
*              search to end of list from position of previous call
30             IF (I1.LE.IFND.AND.INAME.EQ.0) THEN
                  IF (LN.EQ.AVN(I1)) INAME = I1
                  I1 = I1+1
               GOTO 30
               END IF
            END IF

            IF (INAME.EQ.0) THEN
*              match not found to end of list, try beginning
               I1 = 1
35             IF (I1.LE.ISAVE.AND.INAME.EQ.0) THEN
                  IF (LN.EQ.AVN(I1)) INAME = I1
                  I1 = I1+1
               GOTO 35
               END IF
            END IF

            IF (INAME.EQ.0) THEN
*              name not found in list, add to list
               IFND = IFND+1
               IF (IFND.GT.NAMES_MXN) CALL FATALERR
     &            ('OUTDAT','too many variables for output')
               AVN(IFND)  = LN
               INAME      = IFND
               FND(INAME) = 1
            ELSE
*              name found in list
               FND(INAME) = FND(INAME)+1
               IF (FND(INAME).EQ.2) THEN
*                 variable supplied twice, prevent writing to file
                  OK = .FALSE.
                  IF (.NOT.MGIVEN(INAME)) THEN
                     MGIVEN(INAME) = .TRUE.
*                    give warning the second time a variable is entered
*                    do not give warnings on third, fourth etc.
                     I1 = LEN_TRIM (LN)
                     IF (TOSCR) WRITE (*,'(4A)')
     &                 ' WARNING from OUTDAT: variable ',LN(1:I1),
     &                 ' supplied without new independent ',
     &                   LXN(1:ILXN)
                     WRITE (ILU2,'(4A)')
     &                 ' WARNING from OUTDAT: variable ',LN(1:I1),
     &                 ' supplied without new independent ',
     &                   LXN(1:ILXN)
                  END IF
               END IF
            END IF
         END IF

         IF (OK) THEN
            IREC = IREC+1

            WRITE (ILU1,REC=IREC) ' ',INAME, LN, R

*           uncomment for mac absoft fortran compilation
*            WRITE (ILU1,REC=IREC+1) EOFCHR,INAME, LN, R
         END IF

         ISAVE = INAME

      ELSE IF (ITASK.EQ.3) THEN

*        ---------------------------------------
*        selection of output variables for table
*        ---------------------------------------

         IF (ITOLD.EQ.1.OR.ITOLD.EQ.99) THEN
            IF (TOSCR) WRITE (*,'(2A)') ' WARNING from OUTDAT:',
     &        ' wrong task preceeds selection of output'
            WRITE (ILU2,'(2A)') ' WARNING from OUTDAT:',
     &        ' wrong task preceeds selection of output'
            RETURN
         ELSE IF (ITOLD.EQ.2.OR.(ITOLD.GE.4.AND.ITOLD.LE.16)) THEN
*           first time
            INSEL = 0
         END IF

         LN = RN
         CALL UPPERC (LN)

*        lookup name in list
         IF (INSEL.EQ.0) THEN
            I1 = 0
         ELSE
            I1 = IFINDC (ASELN,INSEL,1,INSEL,LN)
         END IF

         IF (I1.EQ.0) THEN
*           name not found in list, add
            INSEL = INSEL+1
            IF (INSEL.GT.NAMES_MXN) CALL FATALERR
     &         ('OUTDAT','too many variables selected')
            ASELN(INSEL) = LN
         END IF

      ELSE IF ((ITASK.GE.4 .AND.ITASK.LE.9).OR.
     &         (ITASK.GE.14.AND.ITASK.LE.19)) THEN

*        --------------------------
*        writing of output table(s)
*        --------------------------

*        error checks on old task
         IF (ITOLD.EQ.-1) THEN
*           recovery of output file, open RES.BIN file
            TOSCR  = .TRUE.
            RECOVR = .TRUE.
            IF (IUNIT.LE.10) CALL FATALERR
     &         ('OUTDAT','illegal unit number')
            ILU2 = IUNIT
            ILU1 = ILU2+1
            CALL FOPENG (ILU1,'RES.BIN','OLD','UD',48,' ')

*           check if output file is open, make new one if it is not

            INQUIRE (UNIT=ILU2, OPENED=OPEND)
            IF (.NOT.OPEND) CALL FOPENG
     &         (ILU2,'RECOVER.DAT','NEW','SF',0,'DEL')

*           find out if end record of last set is in the file
            READ (ILU1,REC=1) RUNTYP,IRUN5
            IF (RUNTYP.EQ.'-') THEN
*              write end record of last set in the file

*              go to start of last set first
               IEREC = 1
               DO I1=1,IRUN5-1
                  ISREC = IEREC+1
                  READ (ILU1,REC=ISREC) RUNDUM,IEREC
               END DO

*              read rest until end_of_file
               IOS = 0
               IR = IEREC+2
45             IF (IOS.EQ.0) THEN
                  READ (ILU1,REC=IR,IOSTAT=IOS) RUNDUM

*                 uncomment for mac absoft fortran compilation
*                  IF (RUNDUM.EQ.EOFCHR) IOS = -1

                  IF (IOS.EQ.0) IR = IR+1

               GOTO 45
               END IF
               WRITE (ILU1, REC=IEREC+1)
     &           ' ',IR-1 ,'....................................',0.
               WRITE (ILU1, REC=1)
     &           '+',IRUN5,'....................................',0.
            END IF

         ELSE IF (ITOLD.EQ.ITASK) THEN

            IF (TOSCR) WRITE (*,'(2A)') ' WARNING from OUTDAT:',
     &        ' new output and previous output have same format'
            WRITE (ILU2,'(2A)') ' WARNING from OUTDAT:',
     &        ' new output and previous output have same format'

         ELSE IF (ITOLD.EQ.99) THEN

            IF (TOSCR) WRITE (*,'(2A)') ' WARNING from OUTDAT:',
     &        ' output table cannot be created, no RES.BIN file'
            WRITE (ILU2,'(2A)') ' WARNING from OUTDAT:',
     &        ' output table cannot be created, no RES.BIN file'
            RETURN

         ELSE IF (ITOLD.EQ.3.OR.ITOLD.EQ.2.OR.ITOLD.EQ.1) THEN

*           previous call was with itask=2, or itask=1
            IEREC = IREC
            WRITE (ILU1, REC=ISREC)
     &        ' ',IREC ,'....................................',0.
            READ  (ILU1, REC=1)     RUNDUM,IRUN5
            WRITE (ILU1, REC=1)
     &        '+',IRUN5,'....................................',0.

         END IF

         ILTASK = MOD (ITASK,10)

         IF (ITASK.GE.14) THEN
*           whole RES.BIN file should be converted
            I6 = 1
         ELSE
*           only last set in RES.BIN file should be converted
            I6 = IRUN5
         END IF

         DO I5=I6,IRUN5

         IF (ITASK.GE.14.OR.ITOLD.EQ.-1) THEN
*           read names from file into AVN array of the set which is
*           converted into an output table. This is not necessary if
*           only the last set has to be converted and this last set
*           was just finished with an itask=2 call (name of independent
*           variable and array AVN are already known !!!).
*           search for first record of set first
            IEREC = 1
            DO I1=1,I5
               ISREC = IEREC+1
               READ (ILU1,REC=ISREC) RUNDUM,IEREC
            END DO

*           read names and determine total number of names
*           read name of independent variable
*           determine length of name of independent variable

            READ (ILU1,REC=ISREC+1) RUNTYP, IRUN3, LXN

            ILXN  = LEN_TRIM (LXN)
            IFND  = 0

            DO IR=ISREC+2,IEREC
               READ (ILU1,REC=IR) RUNDUM, INAME, LN
               IFND = MAX (INAME, IFND)
               IF (INAME.GT.0) AVN(INAME) = LN
            END DO
         END IF

*        assign each selected variable a block number, this is based
*        on the length of the name of the selected variable, when
*        a new name does not fit anymore on the record, a new block is
*        generated
         FULL_BLOCK = .TRUE.
         IBLOK      = 0
         I2         = 1+MAX (COL_WIDTH_MNN+1,ILXN)

         IF (ILTASK.EQ.4) THEN
            LINE_LEN = LINE_LEN_4
         ELSE IF (ILTASK.EQ.5) THEN
            LINE_LEN = LINE_LEN_5
         ELSE IF (ILTASK.EQ.6) THEN
            LINE_LEN = LINE_LEN_4
         ELSE IF (ILTASK.EQ.7) THEN
            LINE_LEN = LINE_LEN_7
         ELSE IF (ILTASK.EQ.8) THEN
            LINE_LEN = LINE_LEN_8
         ELSE IF (ILTASK.EQ.9) THEN
            LINE_LEN = LINE_LEN_9
         END IF

*        assign block numbers to variables
         DO I1=1,IFND

            IF (INSEL.EQ.0) THEN
*              no variables were selected, select each one
               SELECTED = .TRUE.
            ELSE
               ISEL = IFINDC (ASELN,INSEL,1,INSEL,AVN(I1))
               IF (ISEL.GT.0) THEN
*                 variable found in list of selected variables
                  SELECTED = .TRUE.
               ELSE
*                 variable not found in list of selected variables
                  SELECTED = .FALSE.
               END IF
            END IF

            IF (SELECTED) THEN

               IF (FULL_BLOCK) THEN
*                 previous block full
                  IBLOK      = IBLOK+1
                  FULL_BLOCK = .FALSE.
               END IF

*              variable selected
               IAVNL(I1) = LEN_TRIM (AVN(I1)(1:35))
               IF (IAVNL(I1).EQ.0) THEN
                  IF (TOSCR) WRITE (*,'(A)')
     &              ' WARNING from OUTDAT: zero length variable name'
                  WRITE (ILU2,'(A)')
     &              ' WARNING from OUTDAT: zero length variable name'
                  IAVNL(I1) = 1
               END IF

               IF (ILTASK.EQ.6) THEN
                  FULL_BLOCK   = .TRUE.
               ELSE
                  I3 = 1+MAX (COL_WIDTH_MNN,IAVNL(I1))
                  IF (I2+I3.GT.LINE_LEN) THEN
*                    new variable exceeds output file record,
*                    increase block number etc.
                     IBLOK = IBLOK+1
                     I2    = 1+MAX (COL_WIDTH_MNN+1,ILXN)
                  END IF
                  I2 = I2+I3
                  FULL_BLOCK = .FALSE.
               END IF
               BLK(I1) = IBLOK
            ELSE
               BLK(I1) = 0
            END IF
         END DO

*        check number of found blocks
         IF (IBLOK.EQ.0) THEN
            IF (TOSCR) WRITE (*,'(A)')
     &        ' WARNING from OUTDAT: no output values given'
            WRITE (ILU2,'(/,A)')
     &        ' WARNING from OUTDAT: no output values given'
         ELSE IF (IBLOK.GT.1.AND.ILTASK.EQ.8) THEN
            CALL FATALERR ('OUTDAT',
     &      'more than one block with end of run option')
         END IF

*        write comment header stuff
*        --------------------------

         IF (ILTASK.EQ.4.OR.ILTASK.EQ.5.OR.ILTASK.EQ.7) THEN

*           display comment header for normal, tab-delimited output
*           and ICASA output

            COMMCHR = '*'
            IF (ILTASK.EQ.7) COMMCHR = '!'

*           write line to mark start of new run
            WRITE (ILU2,'(A,76A1)') COMMCHR,('-',I1=1,76)

*           write header to output file and possible extra
*           comment lines

            IF (IRUN3.EQ.0) THEN
               WRITE (ILU2,'(2A)')
     &        COMMCHR,' Output table number  :  0 (=first output table)'
            ELSE IF (RUNTYP.EQ.'R') THEN
               WRITE (ILU2,'(2A,I5)')
     &         COMMCHR,' Output from rerun set:',IRUN3
            ELSE IF (RUNTYP.EQ.'N') THEN
               WRITE (ILU2,'(2A,I5)')
     &         COMMCHR,' Output table number  :',IRUN3
            ELSE
               CALL FATALERR ('OUTDAT','unknown run type')
            END IF

            WRITE (ILU2,'(3A,/,3A)')
     &        COMMCHR,' Output table format  : ',TEXT(ILTASK),
     &        COMMCHR,' ',RN

            IF (ILTASK.NE.7) THEN
*              instruct OUTCOM to write comment lines to output file
               CALL OUTCOM ('<PRINT$$$>')
            END IF

*           do not write OUTCOM lines with other output formats,
*           because they will be written with an asterisk instead
*           of with an exclamation mark

         ELSE IF (ILTASK.EQ.8) THEN

*           write comment header for end of run output only once
            IF (FIRST8) THEN
*              write line to mark start of run
               WRITE (ILU2,'(A,76A1)') '*',('-',I1=1,76)

               WRITE (ILU2,'(2A)')
     &         '* Output table format  : ',TEXT(ILTASK)
*              instruct OUTCOM to write comment lines to output file
               CALL OUTCOM ('<PRINT$$$>')
            END IF
         ELSE IF (ILTASK.EQ.9) THEN
*           do not write a comment header for Greenery output
            CONTINUE
         END IF

         DO IB=1,IBLOK

*           search stuff for block number IB
            IFND2 = 0
            DO I1=1,IFND
               IF (BLK(I1).EQ.IB) THEN
*                 variable is in current block, add to list
*                 put pointer in long list
                  IFND2      = IFND2+1
                  SEQ(IFND2) = I1
                  SEQ2(I1)   = IFND2
               END IF
            END DO

*           header with variable names is written dependent on ITASK
            IF (ILTASK.EQ.4.OR.ILTASK.EQ.7) THEN

               WRITE (ILU2,'(A)') ' '
               CHR = ' '

*              write name of independent variable
*              ----------------------------------

*              initialize and add leading space
               LINE    = ' '
               LINE_L  = 1

               IF (ILTASK.EQ.4) THEN
*                 normal table format
                  IF (ILXN.LE.CENTRE+1) THEN
*                    variable name fits left of centre point
                     I3 = LINE_L+1+CENTRE+1-ILXN
                     I4 = LINE_L+CENTRE+1
                     LINE(I3:I4) = LXN(1:ILXN)
                     LINE_L = LINE_L+COL_WIDTH_MNN+1
                     EXTRA_SPX = 0
                  ELSE
*                    variable name extends beyond the centre point
                     LINE(LINE_L+1:LINE_L+ILXN) = LXN(1:ILXN)
                     EXTRA_SPX = MAX (ILXN-COL_WIDTH_MNN+1,0)
                     LINE_L = LINE_L+COL_WIDTH_MNN+1+EXTRA_SPX
                  END IF
               ELSE IF (ILTASK.EQ.7) THEN
*                 ICASA format
                  LINE   = '@'
                  LINE_L = 1
                  CALL ADDSTR (LINE,LINE_L,LXN)
                  CALL ADDSTR (LINE,LINE_L,'>')
                  LINE_L    = 14
                  EXTRA_SPX = 0
               END IF

*              set flag whether extra space is required while writing
               EXTRA_F = EXTRA_SPX.GT.0

*              write names of dependent variables centered above column
               DO I1=1,IFND2
                  I2 = SEQ(I1)

*                 add separating space between variable names
                  LINE_L = LINE_L+1

*                 variable should appear in this block
                  IF (IAVNL(I2).LE.CENTRE) THEN
*                    variable name fits to the left of centre point
                     I3 = LINE_L+1+CENTRE-IAVNL(I2)
                     I4 = LINE_L+CENTRE

                     LINE(I3:I4) = AVN(I2)(1:IAVNL(I2))
                     LINE_L = LINE_L+COL_WIDTH_MNN
                     EXTRA_SP(I1) = 0
                  ELSE
*                    variable name extends beyond centre point
                     LINE(LINE_L+1:LINE_L+IAVNL(I2)) =
     &                  AVN(I2)(1:IAVNL(I2))
                     EXTRA_SP(I1) = MAX (IAVNL(I2)-COL_WIDTH_MNN,0)
                     LINE_L = LINE_L+COL_WIDTH_MNN+EXTRA_SP(I1)
                  END IF

                  IF (EXTRA_SP(I1).GT.0) EXTRA_F = .TRUE.

               END DO

*              write line to file
               WRITE (ILU2,'(A)') LINE(1:LINE_L)
               WRITE (ILU2,'(A)') ' '

*              add a single space to all columns
               EXTRA_SPX = EXTRA_SPX+1
               DO I1=1,IFND2
                  EXTRA_SP(I1) = EXTRA_SP(I1)+1
               END DO

            ELSE IF (ILTASK.EQ.5.OR.(ILTASK.EQ.8.AND.FIRST8)) THEN

*              write tab delimited variable header
               CHR = CHAR (9)

*              write tabs between the names, allow no spaces !!
               LINE   = ' '
               LINE_L = 0
               CALL ADDSTR (LINE,LINE_L,LXN)
               DO I1=1,IFND2
                  CALL ADDSTR (LINE,LINE_L,CHR)
                  CALL ADDSTR (LINE,LINE_L,
     &                         AVN(SEQ(I1))(1:IAVNL(SEQ(I1))))
               END DO

               EXTRA_F = .FALSE.

*              write line to file
               WRITE (ILU2,'(A)') ' '
               WRITE (ILU2,'(A)') LINE(1:LINE_L)
               WRITE (ILU2,'(A)') ' '

               IF (ILTASK.EQ.8.AND.FIRST8) FIRST8 = .FALSE.

            ELSE IF (ILTASK.EQ.6) THEN

*              two column output

               CHR = ' '
               WRITE (ILU2,'(4A,/,A,/,1X,4A)')
     &           '* ',LXN(1:ILXN),CHR,AVN(SEQ(1))(1:IAVNL(SEQ(1))),
     &           ' 1 1 1',
     &           AVN(SEQ(1))(1:IAVNL(SEQ(1))),'(',LXN(1:ILXN),')'

               EXTRA_F = .FALSE.

            ELSE IF (ILTASK.EQ.9.AND.FIRST9) THEN

*              write tab delimited variable header
               CHR = ','

*              write tabs between the names, allow no spaces !!
               LINE   = ' '
               LINE_L = 0
               CALL ADDSTR (LINE,LINE_L,LXN)
               DO I1=1,IFND2
                  CALL ADDSTR (LINE,LINE_L,CHR)
                  CALL ADDSTR (LINE,LINE_L,
     &                         AVN(SEQ(I1))(1:IAVNL(SEQ(I1))))
               END DO

               EXTRA_F = .FALSE.

*              write line to file
               WRITE (ILU2,'(A)') LINE(1:LINE_L)

               IF (ILTASK.EQ.9.AND.FIRST9) FIRST9 = .FALSE.

            END IF

*           initialize output
            YFND = .FALSE.

*           only end of run values, search for last output set
*           (this part was taken from crettp routine of ttselect)

            IF (ILTASK.EQ.8) THEN
               LN = ' '
               IR = IEREC
26             IF (LN.NE.LXN.AND.IREC.GT.ISREC) THEN
                  IR = IR-1
                  READ (ILU1,REC=IR) RUNDUM, INAME, LN
               GOTO 26
               END IF
               ISREC = IR-1
            END IF

            DO IR=ISREC+1,IEREC
*              read next record
               READ (ILU1,REC=IR) RUNDUM, INAME, LN, LV
*              see if variable name is the independent variable
               IF (LN.EQ.LXN) THEN
                  IF (YFND) THEN
                     IF (ICHECK.EQ.IFND2) THEN
*                       no missing variables, write line
*                       directly to file
                        IF (ILTASK.GE.4.AND.ILTASK.LE.8) THEN
                           IF (.NOT.EXTRA_F) THEN
*                             no extra spaces required,
*                             no missing variables
                              WRITE (ILU2,'(1X,1P,G13.6,255(A,G12.5))')
     &                               LVO,(CHR,AVV(I1),I1=1,IFND2)
                           ELSE
*                             extra spaces required
                              WRITE (ILU2,'(A,1P,G13.6,255(A,G12.5))')
     &                           SPACE(1:EXTRA_SPX),LVO,
     &                          (SPACE(1:EXTRA_SP(I1)),AVV(I1),
     &                          I1=1,IFND2)
                           END IF
                        ELSE IF (ILTASK.EQ.9) THEN
                           LINE   = ' '
                           LINE_L = 0
                           CALL ADDINT (LINE,LINE_L,INT (LVO))
                           CALL ADDSTR (LINE,LINE_L,',')
                           CALL ADDINT (LINE,LINE_L,INT (AVV(1)))
                           CALL ADDSTR (LINE,LINE_L,',')
                           CALL ADDINT (LINE,LINE_L,INT (AVV(2)))
                           CALL ADDSTR (LINE,LINE_L,',')
                           CALL ADDINT (LINE,LINE_L,INT (AVV(3)))
                           CALL ADDSTR (LINE,LINE_L,',')
                           DO I1=4,IFND2
                              CALL ADDREA (LINE,LINE_L,AVV(I1),
     &                                     '1P,G12.5')
                              CALL ADDSTR (LINE,LINE_L,',')
                           END DO
                           WRITE (ILU2,'(A)') LINE(1:LINE_L)
                        ELSE
                           CALL FATALERR ('OUTDAT','internal error')
                        END IF
                     ELSE
*                       one or more missing values found write to string
*                       LINE first
                        IF (ILTASK.GE.4.AND.ILTASK.LE.8) THEN
* no new indent level !
                        IF (.NOT.EXTRA_F) THEN
*                          no extra spaces required between columns
                           LINE   = ' '
                           LINE_L = 1
                           CALL ADDREF (LINE,LINE_L,LVO,'1P,G13.6')
                           DO I1=1,IFND2
                              CALL ADDSTF (LINE,LINE_L,CHR)
                              IF (FNDA(I1)) THEN
*                                value was found
                                 CALL ADDREF (LINE,LINE_L,
     &                              AVV(I1),'1P,G12.5')
                              ELSE
*                                value was not found
                                 LINE(LINE_L+CENTRE:LINE_L+CENTRE) = '-'
                                 LINE_L = LINE_L+COL_WIDTH_MNN
                              END IF
                           END DO
                        ELSE
*                          extra spaces required between columns
                           LINE   = ' '
                           LINE_L = EXTRA_SPX
                           CALL ADDREF (LINE,LINE_L,LVO,'1P,G13.6')
                           DO I1=1,IFND2
                              LINE_L = LINE_L+EXTRA_SP(I1)
                              IF (FNDA(I1)) THEN
*                                value was found
                                 CALL ADDREF (LINE,LINE_L,
     &                              AVV(I1),'1P,G12.5')
                              ELSE
*                                value was not found
                                 LINE(LINE_L+CENTRE:LINE_L+CENTRE) = '-'
                                 LINE_L = LINE_L+COL_WIDTH_MNN
                              END IF
                           END DO
                        END IF
* end of no new indent level !
                        ELSE IF (ILTASK.EQ.9) THEN
                           LINE   = ' '
                           LINE_L = 0
                           CALL ADDINT (LINE,LINE_L,INT (LVO))
                           CALL ADDSTR (LINE,LINE_L,',')
                           CALL ADDINT (LINE,LINE_L,INT (AVV(1)))
                           CALL ADDSTR (LINE,LINE_L,',')
                           CALL ADDINT (LINE,LINE_L,INT (AVV(2)))
                           CALL ADDSTR (LINE,LINE_L,',')
                           CALL ADDINT (LINE,LINE_L,INT (AVV(3)))
                           DO I1=4,IFND2
                              CALL ADDSTR (LINE,LINE_L,',')
                              IF (FNDA(I1)) THEN
*                                value was found
                                 CALL ADDREA (LINE,LINE_L,
     &                              AVV(I1),'1P,G12.5')
                              ELSE
*                                value was not found
                                 CALL ADDSTR (LINE,LINE_L,'-')
                              END IF
                           END DO
                        ELSE
                           CALL FATALERR ('OUTDAT','internal error')
                        END IF

                        WRITE (ILU2,'(A)') LINE(1:LINE_L)
                     END IF
*                    initialize search for 'Y' values
                     YFND = .FALSE.
                  END IF

*                 reinitialize things
                  DO I1=1,IFND2
                     FNDA(I1) = .FALSE.
                     AVV(I1)  = -99.
                  END DO
                  ICHECK = 0
                  LVO = LV
               ELSE
*                 record contains 'Y' ; check names I2...I3
                  IF (BLK(INAME).EQ.IB) THEN
                     AVV(SEQ2(INAME))  = LV
                     YFND              = .TRUE.
                     ICHECK            = ICHECK+1
                     FNDA(SEQ2(INAME)) = .TRUE.
                  END IF
               END IF
            END DO

*           write last line at E_O_F
*           unfortunately this section must be identical to the
*           previous one
            IF (YFND) THEN
               IF (ICHECK.EQ.IFND2) THEN
*                 no missing variables, write line
*                 directly to file
                  IF (ILTASK.GE.4.AND.ILTASK.LE.8) THEN
                     IF (.NOT.EXTRA_F) THEN
*                       no extra spaces required,
*                       no missing variables
                        WRITE (ILU2,'(1X,1P,G13.6,255(A,G12.5))')
     &                         LVO,(CHR,AVV(I1),I1=1,IFND2)
                     ELSE
*                       extra spaces required
                        WRITE (ILU2,'(A,1P,G13.6,255(A,G12.5))')
     &                     SPACE(1:EXTRA_SPX),LVO,
     &                    (SPACE(1:EXTRA_SP(I1)),AVV(I1),
     &                    I1=1,IFND2)
                     END IF
                  ELSE IF (ILTASK.EQ.9) THEN
                     LINE   = ' '
                     LINE_L = 0
                     CALL ADDINT (LINE,LINE_L,INT (LVO))
                     CALL ADDSTR (LINE,LINE_L,',')
                     CALL ADDINT (LINE,LINE_L,INT (AVV(1)))
                     CALL ADDSTR (LINE,LINE_L,',')
                     CALL ADDINT (LINE,LINE_L,INT (AVV(2)))
                     CALL ADDSTR (LINE,LINE_L,',')
                     CALL ADDINT (LINE,LINE_L,INT (AVV(3)))
                     CALL ADDSTR (LINE,LINE_L,',')
                     DO I1=4,IFND2
                        CALL ADDREA (LINE,LINE_L,AVV(I1),
     &                               '1P,G12.5')
                        CALL ADDSTR (LINE,LINE_L,',')
                     END DO
                     WRITE (ILU2,'(A)') LINE(1:LINE_L)
                  ELSE
                     CALL FATALERR ('OUTDAT','internal error')
                  END IF
               ELSE
*                 one or more missing values found write to string
*                 LINE first
                  IF (ILTASK.GE.4.AND.ILTASK.LE.8) THEN
* no new indent level !
                  IF (.NOT.EXTRA_F) THEN
*                    no extra spaces required between columns
                     LINE   = ' '
                     LINE_L = 1
                     CALL ADDREF (LINE,LINE_L,LVO,'1P,G13.6')
                     DO I1=1,IFND2
                        CALL ADDSTF (LINE,LINE_L,CHR)
                        IF (FNDA(I1)) THEN
*                          value was found
                           CALL ADDREF (LINE,LINE_L,
     &                        AVV(I1),'1P,G12.5')
                        ELSE
*                          value was not found
                           LINE(LINE_L+CENTRE:LINE_L+CENTRE) = '-'
                           LINE_L = LINE_L+COL_WIDTH_MNN
                        END IF
                     END DO
                  ELSE
*                    extra spaces required between columns
                     LINE   = ' '
                     LINE_L = EXTRA_SPX
                     CALL ADDREF (LINE,LINE_L,LVO,'1P,G13.6')
                     DO I1=1,IFND2
                        LINE_L = LINE_L+EXTRA_SP(I1)
                        IF (FNDA(I1)) THEN
*                          value was found
                           CALL ADDREF (LINE,LINE_L,
     &                        AVV(I1),'1P,G12.5')
                        ELSE
*                          value was not found
                           LINE(LINE_L+CENTRE:LINE_L+CENTRE) = '-'
                           LINE_L = LINE_L+COL_WIDTH_MNN
                        END IF
                     END DO
                  END IF
* end of no new indent level !
                  ELSE IF (ILTASK.EQ.9) THEN
                     LINE   = ' '
                     LINE_L = 0
                     CALL ADDINT (LINE,LINE_L,INT (LVO))
                     CALL ADDSTR (LINE,LINE_L,',')
                     CALL ADDINT (LINE,LINE_L,INT (AVV(1)))
                     CALL ADDSTR (LINE,LINE_L,',')
                     CALL ADDINT (LINE,LINE_L,INT (AVV(2)))
                     CALL ADDSTR (LINE,LINE_L,',')
                     CALL ADDINT (LINE,LINE_L,INT (AVV(3)))
                     DO I1=4,IFND2
                        CALL ADDSTR (LINE,LINE_L,',')
                        IF (FNDA(I1)) THEN
*                          value was found
                           CALL ADDREA (LINE,LINE_L,
     &                        AVV(I1),'1P,G12.5')
                        ELSE
*                          value was not found
                           CALL ADDSTR (LINE,LINE_L,'-')
                        END IF
                     END DO
                  ELSE
                     CALL FATALERR ('OUTDAT','internal error')
                  END IF

                  WRITE (ILU2,'(A)') LINE(1:LINE_L)
               END IF
            END IF
         END DO

*        add some blank lines if table or spreadsheet output was
*        choosen

         IF (ILTASK.EQ.4.OR.ILTASK.EQ.5) WRITE (ILU2,'(/,/,/,1X)')

         END DO

      ELSE IF (ITASK.EQ.99) THEN

*        this option deletes the temporary file
         IF (ITOLD.EQ.99) CALL FATALERR ('OUTDAT',
     &       'Temporary file already deleted')

         CLOSE (ILU1, STATUS='DELETE')

      ELSE IF (ITASK.EQ.0) THEN
*        allow zero task, (occurs with ipform=0 in rkdriv and
*        eudriv drivers)
         CONTINUE
      ELSE
         CALL FATALERR ('OUTDAT','wrong ITASK')
      END IF

*     do not save task when it was a dummy task
      IF (ITASK.NE.0) ITOLD = ITASK

      RETURN
      END
