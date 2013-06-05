      SUBROUTINE FOPENG (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      INTEGER IUNIT,IRECL
      CHARACTER*(*) FILNAM, STATUS, PRIV, TYPE

**    local variables
      INTEGER ILF,ILFIL,ITMP,IWL,IOS, UNLOG
      CHARACTER CHOICE*1,CREAT*4,EXT*3,FFF*11,FTYPE*4
      CHARACTER LFNAME*132,LPRIV*3,LSTAT*3,LTYPE*2
      LOGICAL DELOLD,KNPRIV,OPENU,OPENF,SEQ,THERE,OK
      LOGICAL TOSCR, TOLOG

      SAVE

*     initialize error variables
      IOS = 0

*     desired output type (used in case of 'Unknown' privilege)
      CALL MESSINQ (TOSCR, TOLOG, UNLOG)

*     make filename local and prepare for operating system
      CALL STR_COPY (FILNAM,LFNAME,OK)
      IF (.NOT.OK) THEN
         CALL MESSWRT ('ERROR in FOPENG',
     &    'File name too long for internal buffer')
         CALL FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
      END IF

      CALL FLNAME (LFNAME)
      ILFIL = LEN_TRIM (LFNAME)

*     check unit number
*     -----------------
*     get local copies of status, type and privilege
*     unit number has proper value and is free ?
      IF (IUNIT.LT.10.OR.IUNIT.GT.999) THEN
         CALL MESSWRT ('ERROR in FOPENG',
     &    'Unit number is < 10 or > 999 !')
         CALL FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
      END IF

      INQUIRE (UNIT=IUNIT,OPENED=OPENU)
      IF (OPENU) THEN
*        unit number is in use, get the connected filename
         INQUIRE (UNIT=IUNIT, NAME=LFNAME)
         ILF = LEN_TRIM (LFNAME)
         CALL MESSWRT (
     &    'ERROR in FOPENG: Unit number is already in use for file',
     &     LFNAME(1:ILF))
         CALL FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
      END IF

*     check status
*     ------------
      LSTAT  = STATUS
      CALL UPPERC (LSTAT)

*     Old is always readonly
      IF (LSTAT.EQ.'RDO') LSTAT = 'OLD'

*     simple value check
      IF (LEN_TRIM (STATUS).NE.3.OR.
     &   (LSTAT.NE.'OLD'.AND.LSTAT.NE.'NEW')) THEN
         CALL MESSWRT ('ERROR in FOPENG','illegal file status')
         CALL FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
      END IF

*     check type
*     ----------
      LTYPE  = TYPE
      CALL UPPERC (LTYPE)

      IF (LEN_TRIM (TYPE).NE.2) THEN
*        TYPE should be a two character code
         CALL MESSWRT ('ERROR in FOPENG','Illegal TYPE description')
         CALL FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)

      ELSE IF (INDEX ('FUB',LTYPE(1:1)).EQ.0.AND.
     &         INDEX ('FUB',LTYPE(2:2)).EQ.0) THEN
         CALL MESSWRT ('ERROR in FOPENG','No FORM specified')
         CALL FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)

      ELSE IF (INDEX ('SD',LTYPE(1:1)).EQ.0.AND.
     &         INDEX ('SD',LTYPE(2:2)).EQ.0) THEN
         CALL MESSWRT ('ERROR in FOPENG','No ACCESS specified')
         CALL FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
      END IF

*     get form and check
*     ------------------
      IF (INDEX (LTYPE,'F').GT.0) THEN
*        formatted file
         FFF = 'FORMATTED'
      ELSE IF (INDEX (LTYPE,'U').GT.0) THEN
*        unformatted file
         FFF = 'UNFORMATTED'
      ELSE IF (INDEX (LTYPE,'B').GT.0) THEN
*        binary (IBM only)
         FFF = 'BINARY'
      END IF

      SEQ = INDEX (LTYPE,'S') .GT. 0

*     check record length
*     -------------------
      IF ((.NOT.SEQ .AND. IRECL.LE.0).OR.
     &         (SEQ .AND. IRECL.NE.0)) THEN
         CALL MESSWRT ('ERROR in FOPENG',
     &     'Illegal record length for direct access file')
         CALL FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
      END IF

*     check privilege
*     ---------------
      LPRIV  = PRIV
      CALL UPPERC (LPRIV)

      IF (LSTAT.EQ.'OLD'.AND.LPRIV.NE.' ') THEN
         CALL WARNING ('FOPENG',
     &   'status OLD does not require a privilege')
      ELSE IF (LSTAT.EQ.'NEW') THEN
         KNPRIV = LPRIV.EQ.'DEL' .OR. LPRIV.EQ.'NOD' .OR. LPRIV.EQ.'UNK'

         IF (LEN_TRIM (PRIV).NE.3.OR..NOT.KNPRIV) THEN
            CALL MESSWRT ('ERROR in FOPENG','illegal privilege')
            CALL FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
         END IF
      END IF

*     check filename and file status
*     ------------------------------
      THERE  = .FALSE.
      OPENF  = .FALSE.
      IF (ILFIL.EQ.0) THEN
         CALL MESSWRT ('ERROR in FOPENG','zero length file name')
         CALL FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
      ELSE
         INQUIRE (FILE=LFNAME(1:ILFIL),EXIST=THERE,IOSTAT=IOS)

         IF (IOS.NE.0) THEN
*           error from system, probably from file name
            CALL MESSWRT ('ERROR in FOPENG',
     &    'INQUIRE for file existence not successfull, check IOS value')
            CALL FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
         END IF

         IF (THERE) INQUIRE (FILE=LFNAME(1:ILFIL),OPENED=OPENF)

         IF (THERE.AND.OPENF) THEN
            CALL MESSWRT ('ERROR in FOPENG','File is already open')
            CALL FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
         END IF
      END IF

      IF (LSTAT.EQ.'OLD'.AND..NOT.THERE) THEN
         CALL MESSWRT ('ERROR in FOPENG','File does not exist')
         CALL FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
      END IF

      IF (LSTAT.EQ.'NEW' .AND. THERE) THEN
*        action depends on privilege
         DELOLD = .FALSE.

         IF (LPRIV.EQ.'UNK') THEN
*           interactive choice
            ITMP = 0
10          IF (THERE) THEN
               IF (TOSCR) WRITE (*,'(3A)')
     &           ' File ',LFNAME(1:ILFIL),' already exists'
               IF (TOLOG) WRITE (UNLOG,'(3A)')
     &           ' File ',LFNAME(1:ILFIL),' already exists'
20             CONTINUE
               IF (TOSCR) THEN
                  CALL ENTDCH ('Overwrite (Y/N)','N',CHOICE)
                  CALL UPPERC (CHOICE)
               ELSE
                  CALL MESSWRT ('FOPENG',
     &          'Screen i/o disabled ; cannot ask overwrite permission')
                  CALL FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
               END IF

               IF (CHOICE.EQ.'Y') THEN
*                 delete old file
                  DELOLD = .TRUE.
               ELSE IF (CHOICE.EQ.'N') THEN
*                 old file not deleted, suggest new file name
                  ITMP = ITMP + 1
                  WRITE (EXT,'(I3.3)') ITMP
                  CALL EXTENS (FILNAM,EXT,0,LFNAME)
                  CALL ENTDCH ('Enter new file name',LFNAME,LFNAME)
                  ILFIL = LEN_TRIM (LFNAME)
                  THERE = .FALSE.
                  INQUIRE (FILE=LFNAME(1:ILFIL),EXIST=THERE,IOSTAT=IOS)
                  IF (IOS.NE.0) THEN
*                    error from system, probably from file name
                     CALL MESSWRT ('ERROR in FOPENG',
     &    'INQUIRE for file existence not successfull, check IOS value')
                     CALL FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,
     &                             PRIV,IOS)
                  END IF
                  GOTO 10
               ELSE
*                 illegal choice
                  GOTO 20
               END IF
            END IF

         ELSE IF (LPRIV.EQ.'DEL') THEN
*           delete old file
            DELOLD  = .TRUE.

         ELSE IF (LPRIV.EQ.'NOD') THEN
            CALL MESSWRT ('ERROR in FOPENG',
     &       'Existing file cannot be deleted')
            CALL FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
         END IF

         IF (DELOLD) THEN
*           old file should be deleted
*           delete file as sequential unformatted file ; this does not
*           lead to an error message for direct access files on the Vax
            OPEN (IUNIT,FILE=LFNAME(1:ILFIL),STATUS='OLD',
     &            ACCESS='SEQUENTIAL',FORM='UNFORMATTED')
            CLOSE (IUNIT,STATUS='DELETE',IOSTAT=IOS)

            IF (IOS.NE.0) THEN
*              error from system
               CALL MESSWRT ('ERROR in FOPENG',
     &          'Existing file cannot be deleted')
               CALL FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
            END IF
         END IF
      END IF

*     no errors, open file, !!! MACHINE DEPENDENT !!!
      ILF = LEN_TRIM (FFF)
      CALL LOWERC (LFNAME)

*     =====================================
*     ==== Atari, MS-DOS, Unix, Absoft ====
*     =====================================
      IWL = IRECL
      IF (SEQ) OPEN (IUNIT,FILE=LFNAME(1:ILFIL),STATUS=LSTAT,
     &   ACCESS='SEQUENTIAL',FORM=FFF(1:ILF),IOSTAT=IOS)
      IF (.NOT.SEQ) OPEN (IUNIT,FILE=LFNAME(1:ILFIL),STATUS=LSTAT,
     &   ACCESS='DIRECT',FORM=FFF(1:ILF),RECL=IWL,IOSTAT=IOS)

*     ===================================
*     ======= Fortran on Macintosh ======
*     === Language Systems Fortran 77 ===
*     === Absoft ProFortran 77 and 90 ===
*     ===================================
c      IWL = IRECL
c      IF (FFF.EQ.'FORMATTED') THEN
c*        sequential, formatted files are created as EDIT files
c         CREAT = 'ALFA'
c         FTYPE = 'TEXT'
c      ELSE
c*        some special creator/type combinations depend on extension
c         EXT = ' '
c         IF (ILFIL.GE.4) THEN
c*           get extension
c            IF (LFNAME(ILFIL-3:ILFIL-3).EQ.'.') THEN
c               EXT = LFNAME(ILFIL-2:ILFIL)
c            END IF
c         END IF
c
c         IF (EXT.EQ.'tif') THEN
c*           are created as NIH-Image files
c            CREAT = 'Imag'
c            FTYPE = 'TIFF'
c         ELSE IF (EXT.EQ.'lut') THEN
c*           are created as NIH-Image LUT file (colour table)
c            CREAT = 'Imag'
c            FTYPE = 'ICOL'
c         ELSE IF (EXT.EQ.'pic') THEN
c*           are created as Graphic Converter PICT
c            CREAT = 'GKON'
c            FTYPE = 'PICT'
c         ELSE
c*           otherwise default creator
c            CREAT = 'MPS '
c            FTYPE = 'OBJ '
c         END IF
c      END IF
c
c      IF (SEQ) OPEN (IUNIT,FILE=LFNAME(1:ILFIL),STATUS=LSTAT,
c     &   ACCESS='SEQUENTIAL',FORM=FFF(1:ILF),CREATOR=CREAT,
c     &   FILETYPE=FTYPE,IOSTAT=IOS)
c      IF (.NOT.SEQ) OPEN (IUNIT,FILE=LFNAME(1:ILFIL),STATUS=LSTAT,
c     &   ACCESS='DIRECT',FORM=FFF(1:ILF),RECL=IWL,CREATOR=CREAT,
c     &   FILETYPE=FTYPE,IOSTAT=IOS)

*     =====================================
*     ======= VAX ============= VAX =======
*     =====================================
c      IF (.NOT.SEQ) THEN
c         IF (FFF.EQ.'FORMATTED') THEN
c            IWL = IRECL
c         ELSE IF (FFF.EQ.'UNFORMATTED') THEN
c            IWL = 1 + (IRECL-1)/4
c         END IF
c      END IF

c*     normal open
c      IF (SEQ .AND. FFF.EQ.'FORMATTED'.AND.LSTAT.EQ.'NEW') THEN
c*        sequential formatted file with max record length 512
c         OPEN (IUNIT,FILE=LFNAME(1:ILFIL),STATUS=LSTAT,
c     &    ACCESS='SEQUENTIAL',FORM='FORMATTED',
c     &    BLOCKSIZE=4096,RECL=4096,IOSTAT=IOS,
c     &    CARRIAGECONTROL='LIST')
c      ELSE IF (SEQ .AND. FFF.EQ.'FORMATTED'.AND.LSTAT.EQ.'OLD') THEN
c*        sequential formatted file with max record length 512
c         OPEN (IUNIT,FILE=LFNAME(1:ILFIL),STATUS=LSTAT,
c     &    ACCESS='SEQUENTIAL',FORM='FORMATTED',
c     &    BLOCKSIZE=4096,RECL=4096,IOSTAT=IOS)
c      ELSE IF (SEQ .AND. FFF.EQ.'UNFORMATTED') THEN
c*        sequential unformatted file
c         OPEN (IUNIT,FILE=LFNAME(1:ILFIL),STATUS=LSTAT,
c     &    ACCESS='SEQUENTIAL',FORM='UNFORMATTED',
c     &    BLOCKSIZE=4096,IOSTAT=IOS)
c      ELSE IF (.NOT.SEQ) THEN
c*        direct access file
c         OPEN (IUNIT,FILE=LFNAME(1:ILFIL),STATUS=LSTAT,
c     &    ACCESS='DIRECT',FORM=FFF(1:ILF),RECL=IWL,
c     &    BLOCKSIZE=4096,IOSTAT=IOS)
c      END IF

      IF (IOS.NE.0) THEN
*        error from system
         CALL MESSWRT ('ERROR in FOPENG',
     &    'system error while trying to open file')
         CALL FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
      END IF

      RETURN
      END

      SUBROUTINE FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)

*     error routine of fopeng

      IMPLICIT NONE

*     Formal parameters
      INTEGER IUNIT,IRECL,IOS
      CHARACTER*(*) FILNAM, STATUS, PRIV, TYPE

*     Local variables
      INTEGER ILFIL,UNLOG
      LOGICAL TOSCR, TOLOG
      SAVE

*     desired output type
      CALL MESSINQ (TOSCR, TOLOG, UNLOG)

*     supply info on arguments
      ILFIL = MAX (LEN_TRIM (FILNAM),1)
      IF (TOSCR) WRITE (*,'(/,A,/,A,I5,3(/,2A),/,A,I4,/,2A)')
     & ' Arguments of the CALL to FOPENG leading to this error:',
     & '   Unit             = ',IUNIT,
     & '   File name        =    ',FILNAM(1:ILFIL),
     & '   File status      =    ',STATUS,
     & '   File type        =    ',TYPE,
     & '   Record length    = ',IRECL,
     & '   Delete privilege =    ',PRIV
      IF (TOLOG) WRITE (UNLOG,'(/,A,/,A,I5,3(/,2A),/,A,I4,/,2A)')
     & ' Arguments of the CALL to FOPENG leading to this error:',
     & '   Unit             = ',IUNIT,
     & '   File name        =    ',FILNAM(1:ILFIL),
     & '   File status      =    ',STATUS,
     & '   File type        =    ',TYPE,
     & '   Record length    = ',IRECL,
     & '   Delete privilege =    ',PRIV

      IF (IOS.NE.0) THEN
         IF (TOSCR) WRITE (*,'(A,I6,A)')
     & '   IOSTAT           = ',IOS,
     & ' <-- system I/O status code'
         IF (TOLOG) WRITE (UNLOG,'(A,I6,A)')
     & '   IOSTAT           = ',IOS,
     & ' <-- system I/O status code'
      END IF

      CALL FATALERR (' ',' ')

      RETURN
      END
