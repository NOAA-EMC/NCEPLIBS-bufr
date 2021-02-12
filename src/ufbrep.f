C> @file
C> @brief Read/write one or more data values from/to a data subset.

C> This subroutine reads or writes one or more data values from or to
C> the BUFR data subset that is currently open within the BUFRLIB
C> internal arrays.  The direction of the data transfer is determined
C> by the context of ABS(LUNIN):
C> - If ABS(LUNIN) points to a file that was previously opened for
C>   input using subroutine openbf(), then data values are read from
C>   the current data subset.
C> - If ABS(LUNIN) points to a file that was previously opened for
C>   output using subroutine openbf(), then data values are written to
C>   the current data subset.
C>
C> <p>This subroutine is specifically designed for use with Table B
C> mnemonics which are part of a fixed (i.e. non-delayed) replication
C> sequence, or for mnemonics which are replicated by being directly
C> listed more than once within an overall subset definition.
C> See also subroutines ufbint(),
C> ufbseq() and ufbstp(), which can also be used to read/write one or
C> more data values from/to a data subset but are designed for
C> different use cases.  A more detailed discussion of
C> these different use cases, including examples, is available in
C> [DX BUFR Tables](@ref ufbsubs).
C>
C> @author J. Woollen
C> @date 1994-01-06
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
C> @param[in] STR - character*(*): String of blank-separated
C>                  Table B mnemonics
C>                  in one-to-one correspondence with the number of data
C>                  values that will be read/written from/to the data
C>                  subset within the first dimension of USR (see
C>                  [DX BUFR Tables](@ref dfbftab) for further
C>                  information about Table B mnemonics)
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
C> to a default value of 10E10 via an internal call to subroutine
C> bfrini(), but it can be reset to
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
C> - If ABS(LUNIN) points to a file that is open for input (reading
C> BUFR), there are a few additional special mnemonics that can be
C> included within STR when calling this subroutine, and which in turn
C> will result in special information being returned within the
C> corresponding location in USR.  These special mnemonics are not
C> considered to be part of Table B or Table D and therefore do not
C> need to be definied within the DX or master table file associated
C> with ABS(LUNIN):
C>      - NUL - returns the "missing" value
C>      - IREC - returns the number of the BUFR message within the
C>               file pointed to by ABS(LUNIN) (counting from the
C>               beginning of the file) in which the current data
C>               subset resides
C>      - ISUB - returns the number of the current data subset within
C>               the BUFR message pointed to by IREC, counting from
C>               the beginning of the message
C>
C> <b>Program history log:</b>
C> - 1994-01-06  J. Woollen -- Original author
C> - 1998-07-08  J. Woollen -- Replaced call to Cray library routine ABORT
C>                             with call to new internal routine bort()
C> - 1999-11-18  J. Woollen -- The number of BUFR files which can be
C>                             opened at one time increased from 10 to C32
C>                             (necessary in order to process multiple
C>                             BUFR files under the MPI)
C> - 2003-05-19  J. Woollen -- Disabled the parsing switch which controls
C>                           checking for in the same replication group,
C>                           ufbrep does not need this check, and it
C>                           interferes with what ufbrep can do otherwise
C> - 2003-11-04  S. Bender  -- Added remarks and routine interdependencies
C> - 2003-11-04  D. Keyser  -- Unified/portable for WRF; added history
C>                             documentation; outputs more complete
C>                             diagnostic info when routine terminates
C>                             abnormally, unusual things happen or for
C>                             informational purposes
C> - 2004-08-18  J. Ator    -- Added SAVE for IFIRST1 and IFIRST2 flags
C> - 2009-03-31  J. Woollen -- Add documentation
C> - 2009-04-21  J. Ator    -- Use errwrt()
C> - 2014-12-10  J. Ator    -- Use modules instead of COMMON blocks
C>
      SUBROUTINE UFBREP(LUNIO,USR,I1,I2,IRET,STR)

      USE MODA_USRINT
      USE MODA_MSGCWD

      INCLUDE 'bufrlib.inc'

      COMMON /ACMODE/ IAC
      COMMON /QUIET / IPRT

      CHARACTER*(*) STR
      CHARACTER*128 BORT_STR1,BORT_STR2,ERRSTR
      REAL*8        USR(I1,I2)

      DATA IFIRST1/0/,IFIRST2/0/

      SAVE IFIRST1, IFIRST2

C----------------------------------------------------------------------
C----------------------------------------------------------------------

      IRET = 0

C  CHECK THE FILE STATUS AND I-NODE
C  --------------------------------

      LUNIT = ABS(LUNIO)
      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IM.EQ.0) GOTO 901
      IF(INODE(LUN).NE.INV(1,LUN)) GOTO 902

      IO = MIN(MAX(0,IL),1)
      IF(LUNIO.NE.LUNIT) IO = 0

      IF(I1.LE.0) THEN
         IF(IPRT.GE.0) THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      ERRSTR = 'BUFRLIB: UFBREP - 3rd ARG. (INPUT) IS .LE. 0, ' //
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
      ERRSTR = 'BUFRLIB: UFBREP - 4th ARG. (INPUT) IS .LE. 0, ' //
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

C  INITIALIZE USR ARRAY PRECEEDING AN INPUT OPERATION
C  --------------------------------------------------

      IF(IO.EQ.0) THEN
         DO J=1,I2
         DO I=1,I1
         USR(I,J) = BMISS
         ENDDO
         ENDDO
      ENDIF

C  PARSE OR RECALL THE INPUT STRING - READ/WRITE VALUES
C  ----------------------------------------------------

      IA2 = IAC
      IAC = 1
      CALL STRING(STR,LUN,I1,IO)

C  CALL THE MNEMONIC READER/WRITER
C  -------------------------------

      CALL UFBRP(LUN,USR,I1,I2,IO,IRET)
      IAC = IA2

      IF(IO.EQ.1 .AND. IRET.LT.I2) GOTO 903

      IF(IRET.EQ.0)  THEN
         IF(IO.EQ.0) THEN
            IF(IPRT.GE.1)  THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      ERRSTR = 'BUFRLIB: UFBREP - NO SPECIFIED VALUES READ IN, ' //
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
      ERRSTR = 'BUFRLIB: UFBREP - NO SPECIFIED VALUES WRITTEN OUT, ' //
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
900   CALL BORT('BUFRLIB: UFBREP - BUFR FILE IS CLOSED, IT MUST BE'//
     . ' OPEN')
901   CALL BORT('BUFRLIB: UFBREP - A MESSAGE MUST BE OPEN IN BUFR '//
     . 'FILE, NONE ARE')
902   CALL BORT('BUFRLIB: UFBREP - LOCATION OF INTERNAL TABLE FOR '//
     . 'BUFR FILE DOES NOT AGREE WITH EXPECTED LOCATION IN INTERNAL '//
     . 'SUBSET ARRAY')
903   WRITE(BORT_STR1,'("BUFRLIB: UFBREP - MNEMONIC STRING READ IN IS'//
     . ': ",A)') STR
      WRITE(BORT_STR2,'(18X,"THE NUMBER OF ''LEVELS'' ACTUALLY '//
     . 'WRITTEN (",I3,") LESS THAN THE NUMBER REQUESTED (",I3,") - '//
     . 'INCOMPLETE WRITE")')  IRET,I2
      CALL BORT2(BORT_STR1,BORT_STR2)
      END
