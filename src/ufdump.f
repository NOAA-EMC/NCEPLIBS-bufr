C> @file
C> @brief Print the contents of a data subset.
C>
C> @authors J. Woollen, J. Ator @date 2002-05-14

C> This subroutine prints a verbose listing of the contents of a data
C> subset, including all data values and replicated sequences, as well
C> as the meanings of data values which are code or flag table entries.
C>
C> This subroutine is similar to subroutine ufbdmp(), but it prints
C> different characteristics of each data subset, and in a slightly
C> different format. However, both subroutines can be useful for
C> different diagnostic purposes, and both can also be run
C> interactively to scroll through the contents of a data subset.
C>
C> Logical unit LUNIT should have already been opened for
C> input operations via a previous call to subroutine openbf(), and a
C> BUFR data subset should have already been read into internal arrays
C> via a previous call to one of the
C> [subset-reading subroutines](@ref hierarchy).
C>
C> Except when LUPRT = 0, logical unit LUPRT must already be
C> associated with a filename on the local system, typically via a
C> Fortran "OPEN" statement. When LUPRT = 0, the subroutine will run
C> interactively and print to standard output, scrolling 20 lines at
C> a time and prompting each time whether to quit and return to the
C> application program (by typing 'q' then '&lt;Enter&gt;') or continue
C> scrolling (by typing anything else).
C>
C> In order for the meanings of code and flag table values to be
C> included in the output, a previous call to subroutine codflg()
C> must have been made with argument CF = 'Y'. Otherwise, only the
C> code and flag table values themselves will be printed.
C>
C> @param[in] LUNIT   -- integer: Fortran logical unit number for
C>                       BUFR file
C> @param[in] LUPRT   -- integer: Fortran logical unit number for
C>                       print output
C>                       - 0 = Run interactively, printing to
C>                             standard output
C>
C> @authors J. Woollen, J. Ator @date 2002-05-14
      RECURSIVE SUBROUTINE UFDUMP(LUNIT,LUPRT)

      USE MODV_IM8B

      USE MODA_USRINT
      USE MODA_MSGCWD
      USE MODA_TABABD
      USE MODA_TABLES
      USE MODA_NRV203

      COMMON /TABLEF/ CDMF

      CHARACTER*120 CFMEANG
      CHARACTER*80 FMT
      CHARACTER*64 DESC
      CHARACTER*24 UNIT
      CHARACTER*120 LCHR2
      CHARACTER*20  LCHR,PMISS
      CHARACTER*15 NEMO3
      CHARACTER*10 NEMO,NEMO2,TAGRFE
      CHARACTER*8  NEMOD
      CHARACTER*6  NUMB
      CHARACTER*7  FMTF
      CHARACTER*8  CVAL
      CHARACTER*3  TYPE
      CHARACTER*1  CDMF,TAB,YOU
      EQUIVALENCE  (RVAL,CVAL)
      REAL*8       RVAL
      LOGICAL      TRACK,FOUND,RDRV

      PARAMETER (MXCFDP=5)
      INTEGER   ICFDP(MXCFDP)

      PARAMETER (MXFV=31)
      INTEGER   IFV(MXFV)

      PARAMETER (MXSEQ=10)
      INTEGER   IDXREP(MXSEQ)
      INTEGER   NUMREP(MXSEQ)
      CHARACTER*10 SEQNAM(MXSEQ)
      INTEGER   LSQNAM(MXSEQ)

      PARAMETER (MXLS=10)
      CHARACTER*10 LSNEMO(MXLS)
      INTEGER   LSCT(MXLS)

      DATA PMISS /'             MISSING'/
      DATA YOU /'Y'/

C----------------------------------------------------------------------
C----------------------------------------------------------------------

C  CHECK FOR I8 INTEGERS
C  ---------------------

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84(LUNIT,MY_LUNIT,1)
         CALL X84(LUPRT,MY_LUPRT,1)
         CALL UFDUMP(MY_LUNIT,MY_LUPRT)

         IM8B=.TRUE.
         RETURN
      ENDIF

      NSEQ = 0
      NLS = 0
      LCFMEANG = LEN(CFMEANG)

      IF(LUPRT.EQ.0) THEN
         LUOUT = 6
      ELSE
         LUOUT = LUPRT
      ENDIF

C  CHECK THE FILE STATUS AND I-NODE
C  --------------------------------

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IL.GT.0) GOTO 901
      IF(IM.EQ.0) GOTO 902
      IF(INODE(LUN).NE.INV(1,LUN)) GOTO 903

      WRITE(LUOUT,FMT='(/,2A,/)') 'MESSAGE TYPE ',TAG(INODE(LUN))

C  DUMP THE CONTENTS OF MODULE USRINT FOR UNIT LUNIT
C  -------------------------------------------------

C     If code/flag table details are being printed, and if this is the
C     first subset of a new message, then make sure the appropriate
C     master tables have been read in to memory for this message.

      IF(CDMF.EQ.'Y' .AND. NSUB(LUN).EQ.1) ITMP = IREADMT(LUN)

      DO NV=1,NVAL(LUN)
      IF(LUPRT.EQ.0 .AND. MOD(NV,20).EQ.0) THEN

C  When LUPRT=0, the output will be scrolled, 20 elements at a time
C  ----------------------------------------------------------------

         PRINT*,'(<enter> for MORE, q <enter> to QUIT)'
         READ(5,'(A1)') YOU

C  If the terminal enters "q" followed by "<enter>" after the prompt
C  "(<enter> for MORE, q <enter> to QUIT)", scrolling will end and the
C  subroutine will return to the calling program
C  -------------------------------------------------------------------

         IF(YOU.EQ.'q') THEN
         PRINT*
         PRINT*,'==> You have chosen to stop the dumping of this subset'
         PRINT*
            GOTO 100
         ENDIF
      ENDIF

      NODE = INV (NV,LUN)
      NEMO = TAG (NODE)
      ITYP = ITP (NODE)
      TYPE = TYP (NODE)

      IF(ITYP.GE.1.AND.ITYP.LE.3) THEN
         CALL NEMTAB(LUN,NEMO,IDN,TAB,N)
         if(n>0) then
            NUMB = TABB(N,LUN)(1:6)
            DESC = TABB(N,LUN)(16:70)
            UNIT = TABB(N,LUN)(71:94)
         endif
         RVAL = VAL(NV,LUN)
      ENDIF

      IF((ITYP.EQ.0).OR.(ITYP.EQ.1)) THEN

C        Sequence descriptor or delayed descriptor replication factor

         IF((TYPE.EQ.'REP').OR.(TYPE.EQ.'DRP').OR.
     .      (TYPE.EQ.'DRB').OR.(TYPE.EQ.'DRS')) THEN

C          Print the number of replications

           NSEQ = NSEQ+1
           IF(NSEQ.GT.MXSEQ) GOTO 904
           IF(TYPE.EQ.'REP') THEN
             NUMREP(NSEQ) = IRF(NODE)
           ELSE
             NUMREP(NSEQ) = NINT(RVAL)
           ENDIF
           CALL STRSUC(NEMO,NEMO2,LNM2)
           FMT = '(11X,A,I6,1X,A)'
           WRITE(LUOUT,FMT) NEMO2(1:LNM2), NUMREP(NSEQ), 'REPLICATIONS'

C          How many times is this sequence replicated?

           IF(NUMREP(NSEQ).GT.1) THEN

C            Track the sequence

             SEQNAM(NSEQ) = NEMO2
             LSQNAM(NSEQ) = LNM2
             IDXREP(NSEQ) = 1
           ELSE

C            Don't bother

             NSEQ = NSEQ-1
           ENDIF
         ELSEIF( ((TYPE.EQ.'SEQ').OR.(TYPE.EQ.'RPC').OR.(TYPE.EQ.'RPS'))
     .             .AND. (NSEQ.GT.0) ) THEN

C          Is this one of the sequences being tracked?

           II = NSEQ
           TRACK = .FALSE.
           CALL STRSUC(NEMO,NEMO2,LNM2)
           DO WHILE ((II.GE.1).AND.(.NOT.TRACK))
             IF(NEMO2(1:LNM2).EQ.SEQNAM(II)(2:LSQNAM(II)-1)) THEN
               TRACK = .TRUE.

C              Mark this level in the output

               FMT = '(4X,A,2X,A,2X,A,I6,2X,A)'
               WRITE(LUOUT,FMT) '++++++', NEMO2(1:LNM2),
     .                 'REPLICATION #', IDXREP(II), '++++++'
               IF(IDXREP(II).LT.NUMREP(II)) THEN

C                There are more levels to come

                 IDXREP(II) = IDXREP(II)+1
               ELSE

C                This was the last level for this sequence, so stop
C                tracking it

                 NSEQ = NSEQ-1
               ENDIF
             ELSE
               II = II-1
             ENDIF
           ENDDO
         ENDIF
      ELSEIF(ITYP.EQ.2) THEN

C        Other numeric value

C        First check if this node contains a redefined reference
C        value.  If so, modify the DESC field to label it as such.

         JJ = 1
         RDRV = .FALSE.
         DO WHILE ((JJ.LE.NNRV).AND.(.NOT.RDRV))
            IF (NODE.EQ.INODNRV(JJ)) THEN
               RDRV = .TRUE.
               DESC = 'New reference value for ' // NEMO
               UNIT = ' '
            ELSE
               JJ = JJ + 1
            ENDIF
         ENDDO

C        Check if this element refers to another element via a bitmap.
C        If so, modify the DESC field to identify the referred element.

         NRFE = NRFELM(NV,LUN)
         IF(NRFE.GT.0) THEN
            TAGRFE = TAG(INV(NRFE,LUN))
            JJ = 48
            DO WHILE((JJ.GE.1).AND.(DESC(JJ:JJ).EQ.' '))
               JJ = JJ - 1
            ENDDO
            IF(JJ.LE.33) DESC(JJ+1:JJ+15) = ' for ' // TAGRFE
         ENDIF

C        Now print the value

         IF(IBFMS(RVAL).NE.0) THEN

C           The value is "missing".

            FMT = '(A6,2X,A10,2X,A20,2X,A24,6X,A48)'
            WRITE(LUOUT,FMT) NUMB,NEMO,PMISS,UNIT,DESC
         ELSE
            FMT = '(A6,2X,A10,2X,F20.00,2X,A24,6X,A48)'

C           Based upon the corresponding scale factor, select an
C           appropriate format for the printing of this value.

            WRITE(FMT(19:20),'(I2)') MAX(1,ISC(NODE))
            IF(UNIT(1:4).EQ.'FLAG') THEN

C              Print a listing of the bits corresponding to
C              this value.

               CALL UPFTBV(LUNIT,NEMO,RVAL,MXFV,IFV,NIFV)
               IF(NIFV.GT.0) THEN
                  UNIT(11:11) = '('
                  IPT = 12
                  DO II=1,NIFV
                    ISZ = ISIZE(IFV(II))
                    WRITE(FMTF,'(A2,I1,A4)') '(I', ISZ, ',A1)'
                    IF((IPT+ISZ).LE.24) THEN
                       WRITE(UNIT(IPT:IPT+ISZ),FMTF) IFV(II), ','
                       IPT = IPT + ISZ + 1
                    ELSE
                       UNIT(12:23) = 'MANY BITS ON'
                       IPT = 25
                    ENDIF
                  ENDDO
                  UNIT(IPT-1:IPT-1) = ')'
               ENDIF
            ENDIF

            WRITE(LUOUT,FMT) NUMB,NEMO,RVAL,UNIT,DESC

            IF( (UNIT(1:4).EQ.'FLAG' .OR. UNIT(1:4).EQ.'CODE') .AND.
     .            (CDMF.EQ.'Y') ) THEN

C              Print the meanings of the code and flag values.

               FMT = '(31X,I8,A,A)'
               IF(UNIT(1:4).EQ.'CODE') THEN
                  NIFV = 1
                  IFV(NIFV) = NINT(RVAL)
               ENDIF
               DO II=1,NIFV
                  ICFDP(1) = (-1)
                  IFVD = (-1)
                  CALL SRCHTBF(IDN,IFV(II),ICFDP,MXCFDP,IFVD,
     .                         CFMEANG,LCFMEANG,LCFMG,IERSF)
                  IF(IERSF.EQ.0) THEN
                     WRITE(LUOUT,FMT) IFV(II),' = ',CFMEANG(1:LCFMG)
                  ELSEIF(IERSF.LT.0) THEN
                     WRITE(LUOUT,FMT) IFV(II),' = ',
     .                   '***THIS IS AN ILLEGAL/UNDEFINED VALUE***'
                  ELSE

C                    The meaning of this value is dependent on the
C                    value of another mnemonic in the report.  Look for
C                    that other mnemonic within the report and then use
C                    it and its associated value to retrieve and print
C                    the proper meaning from the code/flag tables.

                     IERFT = (-1)
                     JJ = 0
                     DO WHILE((JJ.LT.IERSF).AND.(IERFT.LT.0))
                        JJ = JJ + 1
                        CALL NUMTBD(LUN,ICFDP(JJ),NEMOD,TAB,IERBD)
                        IF((IERBD.GT.0).AND.(TAB.EQ.'B')) THEN
                           CALL FSTAG(LUN,NEMOD,-1,NV,NOUT,IERFT)
                        ENDIF
                     ENDDO
                     IF(IERFT.EQ.0) THEN
                        IFVD = NINT(VAL(NOUT,LUN))
                        IF(JJ.GT.1) ICFDP(1) = ICFDP(JJ)
                        CALL SRCHTBF(IDN,IFV(II),ICFDP,MXCFDP,IFVD,
     .                               CFMEANG,LCFMEANG,LCFMG,IERSF)
                        IF(IERSF.EQ.0) THEN
                           WRITE(LUOUT,FMT) IFV(II),' = ',
     .                            CFMEANG(1:LCFMG)
                        ENDIF
                     ENDIF
                  ENDIF
               ENDDO
            ENDIF
         ENDIF
      ELSEIF(ITYP.EQ.3) THEN

C        Character (CCITT IA5) value

         NCHR = IBT(NODE)/8

         IF(IBFMS(RVAL).NE.0) THEN
            LCHR = PMISS
         ELSE IF(NCHR.LE.8) THEN
            LCHR = CVAL
         ELSE

C           Track the number of occurrences of this long character string, so
C           that we can properly output each one.

            II = 1
            FOUND = .FALSE.
            DO WHILE((II.LE.NLS).AND.(.NOT.FOUND))
               IF(NEMO.EQ.LSNEMO(II)) THEN
                 FOUND = .TRUE.
               ELSE
                 II = II + 1
               ENDIF
            ENDDO

            IF(.NOT.FOUND) THEN
               NLS = NLS+1
               IF(NLS.GT.MXLS) GOTO 905
               LSNEMO(NLS) = NEMO
               LSCT(NLS) = 1
               NEMO3 = NEMO
            ELSE
               CALL STRSUC(NEMO,NEMO3,LNM3)
               LSCT(II) = LSCT(II) + 1
               WRITE(FMTF,'(A,I1,A)') '(2A,I', ISIZE(LSCT(II)), ')'
               WRITE(NEMO3,FMTF) NEMO(1:LNM3), '#', LSCT(II)
            ENDIF

            CALL READLC(LUNIT,LCHR2,NEMO3)
            IF (ICBFMS(LCHR2,NCHR).NE.0) THEN
               LCHR = PMISS
            ELSE
               LCHR = LCHR2(1:20)
            ENDIF
         ENDIF

         IF ( NCHR.LE.20 .OR. LCHR.EQ.PMISS ) THEN
            LCHR = ADJUSTR(LCHR)
            FMT = '(A6,2X,A10,2X,A20,2X,"(",I2,")",A24,2X,A48)'
            WRITE(LUOUT,FMT) NUMB,NEMO,LCHR,NCHR,UNIT,DESC
         ELSE
            FMT = '(A6,2X,A10,2X,A,2X,"(",I3,")",A23,2X,A48)'
            WRITE(LUOUT,FMT) NUMB,NEMO,LCHR2(1:NCHR),NCHR,UNIT,DESC
         ENDIF
      ENDIF

      ENDDO

      WRITE(LUOUT,3)
3     FORMAT(/' >>> END OF SUBSET <<< '/)

C  EXITS
C  -----

100   RETURN
900   CALL BORT('BUFRLIB: UFDUMP - INPUT BUFR FILE IS CLOSED, IT '//
     . 'MUST BE OPEN FOR INPUT')
901   CALL BORT('BUFRLIB: UFDUMP - INPUT BUFR FILE IS OPEN FOR '//
     . 'OUTPUT, IT MUST BE OPEN FOR INPUT')
902   CALL BORT('BUFRLIB: UFDUMP - A MESSAGE MUST BE OPEN IN INPUT '//
     . 'BUFR FILE, NONE ARE')
903   CALL BORT('BUFRLIB: UFDUMP - LOCATION OF INTERNAL TABLE FOR '//
     . 'INPUT BUFR FILE DOES NOT AGREE WITH EXPECTED LOCATION IN '//
     . 'INTERNAL SUBSET ARRAY')
904   CALL BORT('BUFRLIB: UFDUMP - MXSEQ OVERFLOW')
905   CALL BORT('BUFRLIB: UFDUMP - MXLS OVERFLOW')
      END
