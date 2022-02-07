!> @file
!> @brief Read/write one or more data values from/to a data subset.

!> This subroutine reads or writes one or more data values from or to
!> the BUFR data subset that is currently open within the BUFRLIB
!> internal arrays.  The direction of the data transfer is determined
!> by the context of ABS(LUNIN):
!> - If ABS(LUNIN) points to a file that was previously opened for
!>   input using subroutine openbf(), then data values are read from
!>   the current data subset.
!> - If ABS(LUNIN) points to a file that was previously opened for
!>   output using subroutine openbf(), then data values are written to
!>   the current data subset.
!>
!> <p>This subroutine is specifically designed for use with Table B
!> mnemonics which are part of a delayed-replication sequence, or for
!> which there is no replication at all.  See also subroutines ufbrep(),
!> ufbseq() and ufbstp(), which can also be used to read/write one or
!> more data values from/to a data subset but are designed for
!> different use cases.  A more detailed discussion of
!> these different use cases, including examples, is available in
!> [DX BUFR Tables](@ref ufbsubs).
!>
!> @author J. Woollen
!> @date 1994-01-06
!>
!> <b>Usage:</b> call ufbint( LUNIN, USR, I1, I2, IRET, STR )
!>
!> @param[in] LUNIN   -- integer: Absolute value is Fortran logical
!>                       unit number for BUFR file 
!> @param[in,out] USR -- real*8(*,*): Data values
!>                         - If ABS(LUNIN) was opened for input, then
!>                           USR is output from this subroutine and
!>                           contains data values that were read
!>                           from the current data subset.
!>                         - If ABS(LUNIN) was opened for output, then
!>                           USR is input to this subroutine and
!>                           contains data values that are to be
!>                           written to the current data subset.
!> @param[in] I1 -- integer: Actual first dimension of USR as allocated
!>                  within the calling program
!> @param[in] I2 -- integer:
!>                    - If ABS(LUNIN) was opened for input, then I2
!>                      must be set equal to the actual second dimension
!>                      of USR as allocated within the calling program
!>                    - If ABS(LUNIN) was opened for output, then I2
!>                      must be set equal to the number of replications
!>                      of STR that are to be written to the data subset
!> @param[out] IRET -- integer: Number of replications of STR that were
!>                     actually read/written from/to the data subset
!> @param[in] STR -- character*(*): String of blank-separated
!>                   Table B mnemonics
!>                   in one-to-one correspondence with the number of data
!>                   values that will be read/written from/to the data
!>                   subset within the first dimension of USR (see
!>                   [DX BUFR Tables](@ref dfbftab) for further
!>                   information about Table B mnemonics)
!>
!> <p>It is the user's responsibility to ensure that USR is dimensioned
!> sufficiently large enough to accommodate the number of data values
!> that are to be read from or written to the data subset.  Note also
!> that USR is an array of real*8 values; therefore, any data that are
!> to be written out as character (i.e. CCITT IA5) values in
!> BUFR must be converted from character into real*8 format within the
!> application program before calling this subroutine.  Conversely,
!> when this subroutine is being used to read character values from a
!> data subset, the value that is returned will be in real*8 format
!> and must be converted back into character format by the application
!> program before it can be used as such.  Alternatively, there are
!> different subroutines such as readlc() and writlc() which can be
!> used to read/write character data directly from/to a data subset
!> without the need to convert from/to real*8 format as an intermediate
!> step.
!>
!> <p>Numeric (i.e. non-character) data values within USR are always in
!> the exact units specified for the corresponding mnemonic within the
!> relevant DX or master BUFR table, without any scale or reference
!> values applied.  Specifically, this means that, when writing
!> data values into an output subset, the user only needs to store each
!> respective value into USR using the units specified within the table,
!> and the BUFRLIB software will take care of any necessary scaling or
!> referencing of the value before it is actually encoded into BUFR.
!> Conversely, when reading data values from an input subset, the
!> values returned in USR are already de-scaled and de-referenced and,
!> thus, are already in the exact units that were defined for the
!> corresponding mnemonics within the table.
!>
!> <p>"Missing" values in USR are always denoted by a unique
!> placeholder value.  This placeholder value is initially set
!> to a default value of 10E10_8, but it can be reset to
!> any substitute value of the user's choice via a separate
!> call to subroutine setbmiss().  In any case, and whenever this
!> subroutine is used to read data values from an input subset, any
!> returned value in USR can be easily checked for equivalence to the
!> current placeholder value via a call to function ibfms(), and a
!> positive result means that the value for the corresponding mnemonic
!> was encoded as "missing" in BUFR (i.e. all bits set to 1) within the
!> original data subset.  Conversely, whenever this subroutine
!> is used to write data values to an output subset, the current
!> placeholder value can be obtained via a separate call to function
!> getbmiss(), and the resulting value can then be stored into the
!> USR array whereever the user desires a BUFR "missing" value (i.e.
!> all bits set to 1) to be encoded for the corresponding mnemonic
!> within the output subset.
!>
!> @remarks
!> - If LUNIN < 0, and if ABS(LUNIN) points to a file that is open
!> for output (writing BUFR), then the subroutine will treat the file
!> pointed to by ABS(LUNIN) as though it was open for input (reading
!> BUFR).  This is a special capability for use by some applications
!> that need to read certain values back out from a BUFR file during
!> the same time that it is in the process of being written to.
!> - If ABS(LUNIN) points to a file that is open for input (reading
!> BUFR), STR may contain a Table D mnemonic that is replicated using
!> either 8-bit or 16-bit delayed replication (as noted using
!> replication indicators {} or (), respectively, within the
!> assocated DX BUFR table), and the corresponding location in USR
!> will contain the total number of replications of that mnemonic
!> within the data subset.  Note that, when using this option, the
!> applicable replication indicators must be included in STR 
!> along with the mnemonic itself, as shown in an example in the
!> discussion of [DX BUFR Tables](@ref ufbsubs).
!> - If ABS(LUNIN) points to a file that is open for input (reading
!> BUFR), there are a few additional special mnemonics that can be
!> included within STR when calling this subroutine, and which in turn
!> will result in special information being returned within the
!> corresponding location in USR.  These special mnemonics are not
!> considered to be part of Table B or Table D and therefore do not
!> need to be definied within the DX or master table file associated
!> with ABS(LUNIN):
!>      - NUL - returns the "missing" value
!>      - IREC - returns the number of the BUFR message within the
!>               file pointed to by ABS(LUNIN) (counting from the
!>               beginning of the file) in which the current data
!>               subset resides
!>      - ISUB - returns the number of the current data subset within
!>               the BUFR message pointed to by IREC, counting from
!>               the beginning of the message
!>
!> <b>Program history log:</b>
!> | Date | Programmer | Comments |
!> | -----|------------|----------|
!> | 1994-01-06 | J. Woollen | Original author |
!> | 1996-11-25 | J. Woollen | Modified to add a return code when mnemonics are not found when reading |
!> | 1996-12-17 | J. Woollen | Modified to always initialize USR array to "missing" when BUFR file is being read |
!> | 1998-07-08 | J. Woollen | Replaced call to Cray library routine ABORT with call to new internal routine bort() |
!> | 1999-11-18 | J. Woollen | The number of BUFR files which can be opened at one time increased from 10 to 32 |
!> | 2003-11-04 | S. Bender  | Added remarks and routine interdependencies |
!> | 2003-11-04 | D. Keyser  | Unified/portable for WRF; added documentation; outputs more complete diagnostic info when routine terminates abnormally |
!> | 2004-08-18 | J. Ator    | Added SAVE for IFIRST1 and IFIRST2 flags |
!> | 2009-04-21 | J. Ator    | Use errwrt() |
!> | 2014-12-10 | J. Ator    | Use modules instead of COMMON blocks |
!> | 2022-02-01 | J. Ator    | Converted to module to consolidate _4, _d, and _8 variations into one build |
!>

module subroutine_ufbint

    private
!    public ufbint
    public ufbint, ufbint_isoc
!    public ufbint, ufbint_4_d, ufbint_8

    interface ufbint
        module procedure ufbint_4_d, ufbint_8
    end interface

    contains

    subroutine ufbint_4_d( lunin, usr, i1, i2, iret, str )
!       used when call arguments to ufbint are 4-byte integers

        implicit none

        integer(kind=4), intent(in) :: lunin
        integer(kind=4), intent(in) :: i1, i2
        integer(kind=4), intent(out) :: iret
        real(kind=8), intent(inout) :: usr(i1,i2)
        character(len=*), intent(in) :: str

!        real*8        usr(i1,i2)
!        character*(*) str
        
        integer :: my_lunin, my_i1, my_i2, my_iret

        my_lunin = lunin
        my_i1 = i1
        my_i2 = i2

        call ufbint_body( my_lunin, usr, my_i1, my_i2, my_iret, str )

        iret = my_iret

    end subroutine ufbint_4_d

    subroutine ufbint_8( lunin, usr, i1, i2, iret, str )
!       used when call arguments to ufbint are 8-byte integers

        implicit none

        integer(kind=8), intent(in) :: lunin
        integer(kind=8), intent(in) :: i1, i2
        integer(kind=8), intent(out) :: iret
        real(kind=8), intent(inout) :: usr(i1,i2)
        character(len=*), intent(in) :: str

!        real*8        usr(i1,i2)
!        character*(*) str
        
        integer :: my_lunin, my_i1, my_i2, my_iret

        my_lunin = lunin
        my_i1 = i1
        my_i2 = i2

        call ufbint_body( my_lunin, usr, my_i1, my_i2, my_iret, str )

        iret = my_iret

    end subroutine ufbint_8

    subroutine ufbint_isoc( lunin, usr, i1, i2, iret, str )
!       used when call arguments to ufbint are ISO C types

        use iso_c_binding

        implicit none

        integer(c_int), value, intent(in) :: lunin
        integer(c_int), value, intent(in) :: i1, i2
        integer(c_int), intent(out) :: iret
        real(c_double), pointer, intent(inout) :: usr
!        real(c_double), intent(inout) :: usr(i1,i2)
        character(kind=c_char, len=1), intent(in) :: str
!        character(len=*), intent(in) :: str
        
        integer :: my_lunin, my_i1, my_i2, my_iret

        my_lunin = lunin
        my_i1 = i1
        my_i2 = i2

        call ufbint_body( my_lunin, usr, my_i1, my_i2, my_iret, str )

        iret = my_iret

    end subroutine ufbint_isoc

    subroutine ufbint_body( lunin, usr, i1, i2, iret, str )

      USE MODV_BMISS
      USE MODA_USRINT
      USE MODA_MSGCWD

      COMMON /USRSTR/ NNOD,NCON,NODS(20),NODC(10),IVLS(10),KONS(10)
      COMMON /QUIET / IPRT

      CHARACTER*(*) STR
      CHARACTER*128 BORT_STR1,BORT_STR2,ERRSTR
      REAL*8        USR(I1,I2)

      DATA IFIRST1/0/,IFIRST2/0/

      SAVE IFIRST1, IFIRST2

!----------------------------------------------------------------------
!----------------------------------------------------------------------

      IRET = 0

!  CHECK THE FILE STATUS AND I-NODE
!  --------------------------------

      LUNIT = ABS(LUNIN)
      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IM.EQ.0) GOTO 901
      IF(INODE(LUN).NE.INV(1,LUN)) GOTO 902

      IO = MIN(MAX(0,IL),1)
      IF(LUNIT.NE.LUNIN) IO = 0

      IF(I1.LE.0) THEN
         IF(IPRT.GE.0) THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      ERRSTR = 'BUFRLIB: UFBINT - 3rd ARG. (INPUT) IS .LE. 0, SO RETURN WITH 5th ARG. (IRET) = 0; 6th ARG. (STR) ='
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
      ERRSTR = 'BUFRLIB: UFBINT - 4th ARG. (INPUT) IS .LE. 0, SO RETURN WITH 5th ARG. (IRET) = 0; 6th ARG. (STR) ='
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT(STR)
            IF(IPRT.EQ.0 .AND. IO.EQ.1) THEN
      ERRSTR = 'Note: Only the first occurrence of this WARNING message is printed, there may be more.  To output all such messages,'
      CALL ERRWRT(ERRSTR)
      ERRSTR = 'modify your application program to add "CALL OPENBF(0,''QUIET'',1)" prior to the first call to a BUFRLIB routine.'
      CALL ERRWRT(ERRSTR)
            ENDIF
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
            IFIRST1 = 1
         ENDIF
         GOTO 100
      ENDIF

!  PARSE OR RECALL THE INPUT STRING
!  --------------------------------

      CALL STRING(STR,LUN,I1,IO)

!  INITIALIZE USR ARRAY PRECEEDING AN INPUT OPERATION
!  --------------------------------------------------

      IF(IO.EQ.0) THEN
         DO J=1,I2
         DO I=1,I1
         USR(I,J) = BMISS
         ENDDO
         ENDDO
      ENDIF

!  CALL THE MNEMONIC READER/WRITER
!  -------------------------------

      CALL UFBRW(LUN,USR,I1,I2,IO,IRET)

!  IF INCOMPLETE WRITE TRY TO INITIALIZE REPLICATION SEQUENCE OR RETURN
!  ---------------------------------------------------------------------

      IF(IO.EQ.1 .AND. IRET.NE.I2 .AND. IRET.GE.0) THEN
         CALL TRYBUMP(LUNIT,LUN,USR,I1,I2,IO,IRET)
         IF(IRET.NE.I2) GOTO 903
      ELSEIF(IRET.EQ.-1) THEN
         IRET = 0
      ENDIF

      IF(IRET.EQ.0)  THEN
         IF(IO.EQ.0) THEN
            IF(IPRT.GE.1)  THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      ERRSTR = 'BUFRLIB: UFBINT - NO SPECIFIED VALUES READ IN, SO RETURN WITH 5th ARG. (IRET) = 0; 6th ARG. (STR) ='
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT(STR)
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
            ENDIF
         ELSE
            IF(IPRT.EQ.-1)  IFIRST2 = 1
            IF(IFIRST2.EQ.0 .OR. IPRT.GE.1)  THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      ERRSTR = 'BUFRLIB: UFBINT - NO SPECIFIED VALUES WRITTEN OUT, SO RETURN WITH 5th ARG. (IRET) = 0; 6th ARG. (STR) ='
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT(STR)
      CALL ERRWRT('MAY NOT BE IN THE BUFR TABLE(?)')
               IF(IPRT.EQ.0) THEN
      ERRSTR = 'Note: Only the first occurrence of this WARNING message is printed, there may be more.  To output all such messages,'
      CALL ERRWRT(ERRSTR)
      ERRSTR = 'modify your application program to add "CALL OPENBF(0,''QUIET'',1)" prior to the first call to a BUFRLIB routine.'
      CALL ERRWRT(ERRSTR)
               ENDIF
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
               IFIRST2 = 1
            ENDIF
         ENDIF
      ENDIF

!  EXITS
!  -----

100   RETURN
900   CALL BORT('BUFRLIB: UFBINT - BUFR FILE IS CLOSED, IT MUST BE OPEN')
901   CALL BORT('BUFRLIB: UFBINT - A MESSAGE MUST BE OPEN IN BUFR FILE, NONE ARE')
902   CALL BORT('BUFRLIB: UFBINT - LOCATION OF INTERNAL TABLE FOR BUFR FILE DOES NOT AGREE WITH EXPECTED LOCATION IN INTERNAL SUBSET ARRAY')
903   WRITE(BORT_STR1,'("BUFRLIB: UFBINT - MNEMONIC STRING READ IN IS: ",A)') STR
      WRITE(BORT_STR2,'(18X,"THE NUMBER OF ''LEVELS'' ACTUALLY WRITTEN (",I3,") DOES NOT EQUAL THE NUMBER REQUESTED (",I3,") - INCOMPLETE WRITE")') IRET,I2
      CALL BORT2(BORT_STR1,BORT_STR2)

    end subroutine ufbint_body

end module
