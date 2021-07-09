C> @file
C> @brief Read/write an entire sequence of data values from/to
C> a data subset.

C> This subroutine reads or writes an entire sequence of data values
C> from or to the BUFR data subset that is currently open within the
C> BUFRLIB internal arrays.  The direction of the data transfer is
C> determined by the context of ABS(LUNIN):
C> - If ABS(LUNIN) points to a file that was previously opened for
C>   input using subroutine openbf(), then data values are read from
C>   the current data subset.
C> - If ABS(LUNIN) points to a file that was previously opened for
C>   output using subroutine openbf(), then data values are written to
C>   the current data subset.
C>
C> <p>This subroutine is specifically designed for use with a single
C> Table A or Table D mnemonic.  In the latter case, the mnemonic
C> may be replicated within the overall subset definition, and in
C> which case the subroutine will return all data values within all
C> replications of the sequence defined by the mnemonic.  But in
C> either case, the mnemonic itself may contain, within its own
C> sequence definition, any number of data values defined by Table B
C> mnemonics and/or subsequences of data values defined by other
C> Table D mnemonics, and any such subsequences may themselves be
C> replicated using any manner of fixed or delayed replication.
C> See [DX BUFR Tables](@ref ufbsubs) for more details including
C> an example use case, and see also subroutines ufbint(), ufbrep()
C> and ufbstp() which are also used to read/write one or more data
C> values from/to a data subset but cannot themselves be directly
C> used with Table A or Table D mnemonics.
C>
C> @authors J. Woollen
C> @authors J. Ator
C> @date 2000-09-19
C>
C> @param[in] LUNIN    - integer: Absolute value is Fortran logical
C>                       unit number for BUFR file
C> @param[in,out] USR  - real*8(*,*): Data values
C>                         - If ABS(LUNIN) was opened for input, then
C>                           USR is output from this subroutine and
C>                           contains data values that were read
C>                           from the current data subset.
C>                         - If ABS(LUNIN) was opened for output, then
C>                           USR is input to this subroutine and
C>                           contains data values that are to be
C>                           written to the current data subset.
C> @param[in] I1 - integer: Actual first dimension of USR as allocated
C>                 within the calling program
C> @param[in] I2 - integer:
C>                    - If ABS(LUNIN) was opened for input, then I2
C>                      must be set equal to the actual second dimension
C>                      of USR as allocated within the calling program
C>                    - If ABS(LUNIN) was opened for output, then I2
C>                      must be set equal to the number of replications
C>                      of STR that are to be written to the data subset
C> @param[out] IRET - integer: Number of replications of STR that were
C>                    actually read/written from/to the data subset
C> @param[in] STR - character*(*): String consisting of a single Table A
C>                  or Table D mnemonic whose sequence definition is
C>                  in one-to-one correspondence with the number of data
C>                  values that will be read/written from/to the data
C>                  subset within the first dimension of USR
C>                  (see [DX BUFR Tables](@ref dfbftab) for further
C>                  information about Table A and Table D mnemonics)
C>
C> <p>It is the user's responsibility to ensure that USR is dimensioned
C> sufficiently large enough to accommodate the number of data values
C> that are to be read from or written to the data subset.  Note also
C> that USR is an array of real*8 values; therefore, any data that are
C> to be written out as character (i.e. CCITT IA5) values in
C> BUFR must be converted from character into real*8 format within the
C> application program before calling this subroutine.  Conversely,
C> when this subroutine is being used to read character values from a
C> data subset, the value that is returned will be in real*8 format
C> and must be converted back into character format by the application
C> program before it can be used as such.  Alternatively, there are
C> different subroutines such as readlc() and writlc() which can be
C> used to read/write character data directly from/to a data subset
C> without the need to convert from/to real*8 format as an intermediate
C> step.
C>
C> <p>Numeric (i.e. non-character) data values within USR are always in
C> the exact units specified for the corresponding mnemonic within the
C> relevant DX or master BUFR table, without any scale or reference
C> values applied.  Specifically, this means that, when writing
C> data values into an output subset, the user only needs to store each
C> respective value into USR using the units specified within the table,
C> and the BUFRLIB software will take care of any necessary scaling or
C> referencing of the value before it is actually encoded into BUFR.
C> Conversely, when reading data values from an input subset, the
C> values returned in USR are already de-scaled and de-referenced and,
C> thus, are already in the exact units that were defined for the
C> corresponding mnemonics within the table.
C>
C> <p>"Missing" values in USR are always denoted by a unique
C> placeholder value.  This placeholder value is initially set
C> to a default value of 10E10_8, but it can be reset to
C> any substitute value of the user's choice via a separate
C> call to subroutine setbmiss().  In any case, and whenever this
C> subroutine is used to read data values from an input subset, any
C> returned value in USR can be easily checked for equivalence to the
C> current placeholder value via a call to function ibfms(), and a
C> positive result means that the value for the corresponding mnemonic
C> was encoded as "missing" in BUFR (i.e. all bits set to 1) within the
C> original data subset.  Conversely, whenever this subroutine
C> is used to write data values to an output subset, the current
C> placeholder value can be obtained via a separate call to function
C> getbmiss(), and the resulting value can then be stored into the
C> USR array whereever the user desires a BUFR "missing" value (i.e.
C> all bits set to 1) to be encoded for the corresponding mnemonic
C> within the output subset.
C>
C> @remarks
C> - If LUNIN < 0, and if ABS(LUNIN) points to a file that is open
C> for output (writing BUFR), then the subroutine will treat the file
C> pointed to by ABS(LUNIN) as though it was open for input (reading
C> BUFR).  This is a special capability for use by some applications
C> that need to read certain values back out from a BUFR file during
C> the same time that it is in the process of being written to.
C> - If ABS(LUNIN) points to a file that is open for output
C> (writing BUFR), and if the data values to be written are part of
C> a sequence replicated using delayed replication, then a call to
C> subroutine drfini() must be made prior to calling this subroutine,
C> in order to pre-allocate the necessary internal array space for
C> the number of replications of the sequence.
C>
C> <b>Program history log:</b>
C> - 2000-09-19  J. Woollen -- Original author
C> - 2002-05-14  J. Woollen -- Improved generality; previously ufbseq
C>                           would not recognize compressed delayed
C>                           replication as a legitimate data structure
C> - 2003-05-19  J. Woollen -- Corrected the logic array of exit
C>                           conditions for the subroutine; previously,
C>                           in some cases, proper exits were missed,
C>                           generating bogus error messages, because of
C>                           several miscellaneous bugs which are now
C>                           removed
C> - 2003-11-04  S. Bender  -- Added remarks and routine interdependencies
C> - 2003-11-04  D. Keyser  -- Unified/portable for WRF; added history
C>                             documentation; outputs more complete
C>                             diagnostic info when routine terminates
C>                             abnormally, unusual things happen or for
C>                             informational purposes
C> - 2004-08-18  J. Ator    -- Added SAVE for IFIRST1 and IFIRST2 flags
C> - 2007-01-19  J. Ator    -- Replaced call to parseq with call to parstr()
C> - 2009-04-21  J. Ator    -- Use errwrt()
C> - 2014-09-10  J. Ator    -- Fix bug involving nested delayed replication
C>                           where first replication of outer sequence
C>                           does not contain a replication of the inner
C>                           sequence
C> - 2014-12-10  J. Ator    -- Use modules instead of COMMON blocks
C> - 2020-03-06  J. Ator    -- No longer abort when reading data and number
C>                           of available levels is greater than I2;
C>                           instead just return first I2 levels and
C>                           print a diagnostic message
C>
      SUBROUTINE UFBSEQ(LUNIN,USR,I1,I2,IRET,STR)

      USE MODV_BMISS
      USE MODA_USRINT
      USE MODA_MSGCWD
      USE MODA_TABLES

      PARAMETER (MTAG=10)

      COMMON /QUIET / IPRT

      CHARACTER*(*) STR
      CHARACTER*156 BORT_STR
      CHARACTER*128 ERRSTR
      CHARACTER*10  TAGS(MTAG)
      REAL*8        USR(I1,I2)

      DATA IFIRST1/0/,IFIRST2/0/

      SAVE IFIRST1, IFIRST2

C----------------------------------------------------------------------
C----------------------------------------------------------------------

      IRET = 0

C  CHECK THE FILE STATUS AND I-NODE
C  --------------------------------

      LUNIT = ABS(LUNIN)
      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IM.EQ.0) GOTO 901

      IO = MIN(MAX(0,IL),1)
      IF(LUNIT.NE.LUNIN) IO = 0

      IF(I1.LE.0) THEN
         IF(IPRT.GE.0) THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      ERRSTR = 'BUFRLIB: UFBSEQ - 3rd ARG. (INPUT) IS .LE. 0, ' //
     .   'SO RETURN WITH 5th ARG. (IRET) = 0; 6th ARG. (STR) ='
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT(STR)
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
         ENDIF
         GOTO 100
      ELSEIF(I2.LE.0) THEN
         IF(IPRT.EQ.-1)  IFIRST1 = 1
         IF(IO.EQ.0 .OR. IFIRST1.EQ.0 .OR. IPRT.GE.1)  THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      ERRSTR = 'BUFRLIB: UFBSEQ - 4th ARG. (INPUT) IS .LE. 0, ' //
     .   'SO RETURN WITH 5th ARG. (IRET) = 0; 6th ARG. (STR) ='
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT(STR)
            IF(IPRT.EQ.0 .AND. IO.EQ.1) THEN
      ERRSTR = 'Note: Only the first occurrence of this WARNING ' //
     .   'message is printed, there may be more.  To output all ' //
     .   'such messages,'
      CALL ERRWRT(ERRSTR)
      ERRSTR = 'modify your application program to add ' //
     .   '"CALL OPENBF(0,''QUIET'',1)" prior to the first call ' //
     .   'to a BUFRLIB routine.'
      CALL ERRWRT(ERRSTR)
            ENDIF
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
            IFIRST1 = 1
         ENDIF
         GOTO 100
      ENDIF

C  CHECK FOR VALID SEQUENCE AND SEQUENCE LENGTH ARGUMENTS
C  ------------------------------------------------------

      CALL PARSTR(STR,TAGS,MTAG,NTAG,' ',.TRUE.)
      IF(NTAG.LT.1) GOTO 902
      IF(NTAG.GT.1) GOTO 903
      IF(I1.LE.0) GOTO 904
      IF(I2.LE.0) GOTO 905
      IF(INODE(LUN).NE.INV(1,LUN)) GOTO 906


C  INITIALIZE USR ARRAY PRECEEDING AN INPUT OPERATION
C  --------------------------------------------------

      IF(IO.EQ.0) THEN
         DO J=1,I2
         DO I=1,I1
         USR(I,J) = BMISS
         ENDDO
         ENDDO
      ENDIF


C  FIND THE PARAMETERS OF THE SPECIFIED SEQUENCE
C  ---------------------------------------------

      DO NODE=INODE(LUN),ISC(INODE(LUN))
      IF(STR.EQ.TAG(NODE)) THEN
         IF(TYP(NODE).EQ.'SEQ'.OR.TYP(NODE).EQ.'RPC') THEN
            INS1 = 1
5           INS1 = INVTAG(NODE,LUN,INS1,NVAL(LUN))
            IF(INS1.EQ.0) GOTO 200
            IF(TYP(NODE).EQ.'RPC'.AND.VAL(INS1,LUN).EQ.0.) THEN
               INS1 = INS1+1
               GOTO 5
            ENDIF
            INS2 = INVTAG(NODE,LUN,INS1+1,NVAL(LUN))
            IF(INS2.EQ.0) INS2 = 10E5
            NODS = NODE
            DO WHILE(LINK(NODS).EQ.0.AND.JMPB(NODS).GT.0)
            NODS = JMPB(NODS)
            ENDDO
            IF(LINK(NODS).EQ.0) THEN
               INSX = NVAL(LUN)
            ELSEIF(LINK(NODS).GT.0) THEN
               INSX = INVWIN(LINK(NODS),LUN,INS1+1,NVAL(LUN))-1
            ENDIF
            INS2 = MIN(INS2,INSX)
         ELSEIF(TYP(NODE).EQ.'SUB') THEN
            INS1 = 1
            INS2 = NVAL(LUN)
         ELSE
            GOTO 907
         ENDIF
         NSEQ = 0
         DO ISQ=INS1,INS2
         ITYP = ITP(INV(ISQ,LUN))
         IF(ITYP.GT.1) NSEQ = NSEQ+1
         ENDDO
         IF(NSEQ.GT.I1) GOTO 908
         GOTO 1
      ENDIF
      ENDDO

      GOTO 200

C  FRAME A SECTION OF THE BUFFER - RETURN WHEN NO FRAME
C  ----------------------------------------------------

1     INS1 = INVTAG(NODE,LUN,INS1,NVAL(LUN))
      IF(INS1.GT.NVAL(LUN)) GOTO 200
      IF(INS1.GT.0) THEN
         IF(TYP(NODE).EQ.'RPC'.AND.VAL(INS1,LUN).EQ.0.) THEN
            INS1 = INS1+1
            GOTO 1
         ELSEIF(IO.EQ.0.AND.IRET+1.GT.I2) THEN
            IF(IPRT.GE.0)  THEN
      CALL ERRWRT('++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++')
      WRITE ( UNIT=ERRSTR, FMT='(A,I5,A,A,A)' )
     . 'BUFRLIB: UFBSEQ - INCOMPLETE READ; ONLY THE FIRST ', I2,
     . ' (=4TH INPUT ARG.) ''LEVELS'' OF INPUT MNEMONIC ', TAGS(1),
     . ' WERE READ'
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT('++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++')
      CALL ERRWRT(' ')
            ENDIF
            GOTO 200
         ENDIF
      ELSEIF(INS1.EQ.0) THEN
         IF(IO.EQ.1.AND.IRET.LT.I2) GOTO 910
      ELSE
         GOTO 911
      ENDIF

      IF(INS1.EQ. 0) GOTO 200
      IF(IRET.EQ.I2) GOTO 200

      IRET = IRET+1
      INS1 = INS1+1

C  READ/WRITE USER VALUES
C  ----------------------

      J = INS1
      DO I=1,NSEQ
      DO WHILE(ITP(INV(J,LUN)).LT.2)
      J = J+1
      ENDDO
      IF(IO.EQ.0) USR(I,IRET) = VAL(J,LUN )
      IF(IO.EQ.1) VAL(J,LUN ) = USR(I,IRET)
      J = J+1
      ENDDO

C  CHECK FOR NEXT FRAME
C  --------------------

      GOTO 1

200   CONTINUE

      IF(IRET.EQ.0)  THEN
         IF(IO.EQ.0) THEN
            IF(IPRT.GE.1)  THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      ERRSTR = 'BUFRLIB: UFBSEQ - NO SPECIFIED VALUES READ IN, ' //
     .   'SO RETURN WITH 5th ARG. (IRET) = 0; 6th ARG. (STR) ='
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT(STR)
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
            ENDIF
         ELSE
            IF(IPRT.EQ.-1)  IFIRST2 = 1
            IF(IFIRST2.EQ.0 .OR. IPRT.GE.1)  THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      ERRSTR = 'BUFRLIB: UFBSEQ - NO SPECIFIED VALUES WRITTEN OUT, ' //
     .   'SO RETURN WITH 5th ARG. (IRET) = 0; 6th ARG. (STR) ='
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT(STR)
      CALL ERRWRT('MAY NOT BE IN THE BUFR TABLE(?)')
               IF(IPRT.EQ.0) THEN
      ERRSTR = 'Note: Only the first occurrence of this WARNING ' //
     .   'message is printed, there may be more.  To output all ' //
     .   'such messages,'
      CALL ERRWRT(ERRSTR)
      ERRSTR = 'modify your application program to add ' //
     .   '"CALL OPENBF(0,''QUIET'',1)" prior to the first call ' //
     .   'to a BUFRLIB routine.'
      CALL ERRWRT(ERRSTR)
               ENDIF
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
               IFIRST2 = 1
            ENDIF
         ENDIF
      ENDIF

C  EXITS
C  -----

100   RETURN
900   CALL BORT('BUFRLIB: UFBSEQ - BUFR FILE IS CLOSED, IT MUST BE'//
     . ' OPEN')
901   CALL BORT('BUFRLIB: UFBSEQ - A MESSAGE MUST BE OPEN IN BUFR '//
     . 'FILE, NONE ARE')
902   WRITE(BORT_STR,'("BUFRLIB: UFBSEQ - THE INPUT STRING (",A,") '//
     . 'DOES NOT CONTAIN ANY MNEMONICS!!")') STR
      CALL BORT(BORT_STR)
903   WRITE(BORT_STR,'("BUFRLIB: UFBSEQ - THERE CANNOT BE MORE THAN '//
     . 'ONE MNEMONIC IN THE INPUT STRING (",A,") (HERE THERE ARE ",I3'//
     . ',")")') STR,NTAG
      CALL BORT(BORT_STR)
904   WRITE(BORT_STR,'("BUFRLIB: UFBSEQ - THIRD ARGUMENT (INPUT) MUST'//
     . ' BE .GT. ZERO (HERE IT IS",I4,") - INPUT MNEMONIC IS ",A)')
     . I1,TAGS(1)
      CALL BORT(BORT_STR)
905   WRITE(BORT_STR,'("BUFRLIB: UFBSEQ - FOURTH ARGUMENT (INPUT) '//
     . 'MUST BE .GT. ZERO (HERE IT IS",I4,") - INPUT MNEMONIC IS ",A)')
     . I2,TAGS(1)
      CALL BORT(BORT_STR)
906   CALL BORT('BUFRLIB: UFBSEQ - LOCATION OF INTERNAL TABLE FOR '//
     . 'BUFR FILE DOES NOT AGREE WITH EXPECTED LOCATION IN INTERNAL '//
     . 'SUBSET ARRAY')
907   WRITE(BORT_STR,'("BUFRLIB: UFBSEQ - INPUT MNEMONIC ",A," MUST '//
     . 'BE A SEQUENCE (HERE IT IS TYPE """,A,""")")') TAGS(1),TYP(NODE)
      CALL BORT(BORT_STR)
908   WRITE(BORT_STR,'("BUFRLIB: UFBSEQ - INPUT SEQ. MNEM. ",A,'//
     . '" CONSISTS OF",I4," TABLE B MNEM., .GT. THE MAX. SPECIFIED IN'//
     . ' (INPUT) ARGUMENT 3 (",I3,")")') TAGS(1),NSEQ,I1
      CALL BORT(BORT_STR)
910   WRITE(BORT_STR,'("BUFRLIB: UFBSEQ - NO. OF ''LEVELS'' WRITTEN '//
     . '(",I5,") .LT. NO. REQUESTED (",I5,") - INCOMPLETE WRITE '//
     . '(INPUT MNEMONIC IS ",A,")")')  IRET,I2,TAGS(1)
      CALL BORT(BORT_STR)
911   WRITE(BORT_STR,'("BUFRLIB: UFBSEQ - VARIABLE INS1 MUST BE .GE. '//
     . 'ZERO, HERE IT IS",I4," - INPUT MNEMONIC IS ",A)') INS1,TAGS(1)
      CALL BORT(BORT_STR)
      END
