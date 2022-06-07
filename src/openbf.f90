!> @file
!> @brief Connect a new system file to the BUFRLIB software for reading or writing BUFR messages.

!> This subroutine connects a new file to the BUFRLIB software for input or output operations.
!>
!> @authors J. Woollen
!> @authors J. Ator
!> @authors D. Keyser
!> @date 1994-01-06
!>
!> <b>Usage:</b> call openbf( LUNIT, IO, LUNDX )
!>
!> @param[in] LUNIT   -- integer: Fortran logical unit number for BUFR file (unless IO is set to 'FIRST' or 'QUIET', in
!>                       which case this is a dummy argument)
!> @param[in] IO      -- character*(*): flag indicating how LUNIT is to be used by the software:
!>                 -   'IN' = input operations with table processing
!>                 -   'INX' = input operations w/o table processing
!>                 -   'OUX' = output operations w/o table processing
!>                 -   'OUT' = output operations with table processing
!>                 -  'SEC3' = same as 'IN', except use Section 3 of input messages for decoding rather than DX BUFR
!>                             table information from LUNDX; in this case LUNDX is ignored, and user must provide 
!>                             appropriate [master BUFR tables](@ref dfbfmstab) within the directory specified by a
!>                             subsequent call to subroutine mtinfo()
!>                 -  'NODX' = same as 'OUT', except don't write DX BUFR table messages to LUNIT
!>                 -   'APN' = same as 'NODX', except begin writing at end of file ("append")
!>                 -   'APX' = same as 'APN', except backspace before appending
!>                 -   'NUL' = same as 'OUT', except don't write any messages whatsoever to LUNIT (e.g. when
!>                             subroutine writsa() is to be used)
!>                 -  'INUL' = same as 'IN', except don't read any messages whatsoever from LUNIT (e.g. when
!>                             subroutine readerme() is to be used)
!>                 - 'QUIET' = LUNIT is ignored; this is an indicator that the value for IPRT in COMMON block /QUIET/
!>                             is being reset to the value in LUNDX
!>                 - 'FIRST' = LUNIT and LUNDX are ignored; this is an indicator to initialize the BUFRLIB software,
!>                             in case this subroutine was never previously called
!> @param[in] LUNDX   -- integer:
!>                 - If IO is not set to 'FIRST' or 'QUIET' = Fortran logical unit number containing DX BUFR table information
!>                   to be used in reading/writing from/to LUNIT (depending on the case).  This value may be set equal to LUNIT
!>                   if DX BUFR table information is already embedded in LUNIT.
!>                 - If IO is set to 'QUIET' = indicator for degree of printout:
!>                      - -1 = no printout except for ABORT messages
!>                      -  0 = limited printout (default)
!>                      -  1 = all warning messages are printed out
!>                      -  2 = all warning and informational messages are printed out
!>
!> <p>The logical unit numbers LUNIT and LUNDX must already be associated
!> with actual filenames on the local system, typically via a Fortran "OPEN"
!> statement. Multiple logical units can be connected to the BUFRLIB software
!> at any one time.
!>
!> <p>The argument IO is a character string describing how the file connected to
!> LUNIT will be used, e.g. 'IN' is used to access an existing file of BUFR
!> messages for input (i.e. reading/decoding BUFR), and 'OUT' is used to access
!> a new file for output (i.e. writing/encoding BUFR). An option 'APX' is also
!> available which behaves like 'OUT', except that output is then appended to
!> an existing BUFR file rather than creating a new one from scratch, and there
!> are also some additional options 'NUL' and 'NODX' which can likewise be used
!> instead of 'OUT' for some very special cases as needed. There's also an
!> option 'SEC3' which can be used in place of 'IN' for certain cases when the
!> user is attempting to read BUFR messages whose content and descriptor layout
!> are unknown in advance. However, all of these additional options are
!> basically just variations of 'IN' or 'OUT', again depending on whether the
!> intent is to read or write BUFR messages from the file connected to LUNIT.
!> The only exceptions are when IO = 'FIRST' or 'QUIET'.  When IO = 'FIRST',
!> the subroutine simply checks whether it has already been called from within
!> the application program and, if not, goes ahead and initializes the library
!> without actually connecting any files in LUNIT or LUNDX.
!> Alternatively, when IO = 'QUIET', the subroutine simply sets or resets the
!> internal print verbosity switch to the value of input argument LUNDX,
!> overriding its previous value and/or its internal default value of 0.
!>
!> <p>The third and final call argument LUNDX identifies the logical unit which
!> contains the definition of the DX BUFR tables to be associated with unit
!> LUNIT.  Except when IO = 'SEC3', every BUFR file that is linked to the BUFRLIB
!> software must have a DX BUFR tables file associated with it, and these tables
!> may be defined within a separate ASCII text file
!> (see [Description and Format of DX BUFR Tables](@ref dfbftab) for more info.)
!> or, in the case of an existing BUFR file, may be embedded within the first few
!> BUFR messages of the file itself, and in which case the user can denote this
!> to the subroutine by setting LUNDX to the same value as LUBFR.
!>
!> @remarks
!> - When an existing BUFR file is accessed for input (i.e. reading/decoding BUFR),
!> the associated DX BUFR tables defined by LUNDX are stored internally within
!> the BUFRLIB software and are referenced during all subsequent processing of
!> the file. Likewise, when a file is accessed for output (i.e. writing/encoding
!> BUFR), the associated DX BUFR tables are still stored internally for subsequent
!> reference; however, the output file itself is also initialized by writing the
!> BUFR table information (as one or more BUFR messages) to the beginning of the
!> file, except when IO = 'NODX', and in which case the writing of these
!> additional messages is suppressed.
!> - As noted above, 'SEC3' is the only value of IO (other than 'QUIET') where it's
!> not necessary to provide pre-defined DX BUFR tables via LUNDX.  Instead, this
!> option instructs the BUFRLIB software to unpack the data description section
!> (Section 3) from each BUFR message it reads and then decode the contents
!> accordingly. In this case, it's necessary to provide a set of BUFR master
!> tables containing listings of all possible BUFR descriptors
!> (see [Description and Format of master BUFR Tables](@ref dfbfmstab) for more
!> info.), but otherwise no prior knowledge is required of the contents of the
!> messages to be decoded.
!>
!> <b>Program history log:</b>
!> | Date | Programmer | Comments |
!> | -----|------------|----------|
!> | 1994-01-06 | J. Woollen | Original author |
!> | 1998-07-08 | J. Woollen | Replaced call to Cray library routine ABORT with call to new internal routine bort() |
!> | 1999-11-18 | J. Woollen | The number of BUFR files which can be opened at one time increased from 10 to 32 |
!> | 2003-11-04 | J. Ator    | Added IO='NUL' option to prevent later writing to BUFR file in LUNIT; added documentation |
!> | 2003-11-04 | S. Bender  | Added remarks and routine interdependencies |
!> | 2003-11-04 | D. Keyser  | Unified/portable for WRF; added documentation; outputs more complete diagnostic info when routine terminates abnormally |
!> | 2004-08-18 | J. Ator    | Added SAVE for IFIRST flag and IO="NODX" option |
!> | 2005-11-29 | J. Ator    | Added COMMON /MSGFMT/ and ichkstr() call |
!> | 2009-03-23 | J. Ator    | Added IO='SEC3' option; removed call to posapn; clarified comments; use errwrt() |
!> | 2010-05-11 | J. Ator    | Added COMMON /STCODE/ |
!> | 2012-06-18 | J. Ator    | Added IO='INUL' option |
!> | 2012-09-15 | J. Woollen | Modified for C/I/O/BUFR interface; use INQUIRE to obtain filename; use openrb(), openwb() and openab(); add IO types 'INX' and 'FIRST' |
!> | 2014-11-07 | J. Ator    | Allow dynamic allocation of certain arrays |
!> | 2015-03-03 | J. Ator    | Use MODA_IFOPBF instead of IFIRST |
!> | 2022-06-01 | J. Ator    | Converted to module to consolidate _4, _d, and _8 variations into one build |
!>

module subroutine_openbf

    private
    public openbf

    interface openbf
        module procedure openbf_4_d, openbf_8
    end interface

    contains

    subroutine openbf_4_d( lunit, io, lundx )
!       used when call arguments to openbf are 4-byte integers

        implicit none

        integer(kind=4), intent(in) :: lunit, lundx
        character(len=*), intent(in) :: io

        integer :: my_lunit, my_lundx 

        my_lunit = lunit
        my_lundx = lundx

        call openbf_body( my_lunit, io, my_lundx )

    end subroutine openbf_4_d

    subroutine openbf_8( lunit, io, lundx )
!       used when call arguments to openbf are 8-byte integers

        implicit none

        integer(kind=8), intent(in) :: lunit, lundx
        character(len=*), intent(in) :: io

        integer :: my_lunit, my_lundx 

        my_lunit = lunit
        my_lundx = lundx

        call openbf_body( my_lunit, io, my_lundx )

    end subroutine openbf_8

    subroutine openbf_body( lunit, io, lundx )

      USE MODV_IFOPBF
      USE MODA_MSGCWD
      USE MODA_STBFR
      USE MODA_SC3BFR
      USE MODA_LUSHR
      USE MODA_NULBFR
      USE MODA_STCODE

      COMMON /QUIET / IPRT

      CHARACTER*(*) IO
      CHARACTER*255 FILENAME,FILEACC   
      CHARACTER*132 ERRSTR
      CHARACTER*128 BORT_STR
      CHARACTER*28  CPRINT(0:3)
      CHARACTER*1   BSTR(4)

      DATA          CPRINT/ &
       ' (only ABORTs)              ', &
       ' (limited - default)        ', &
       ' (all warnings)             ', &
       ' (all warning+informational)'/

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

!     If this is the first call to this subroutine, initialize
!     IPRT in /QUIET/ as 0 (limited printout - except for abort
!     messages)

      IF(IFOPBF.EQ.0) IPRT = 0

      IF(IO.EQ.'QUIET') THEN
!  .... override previous IPRT value (printout indicator)
         IF(LUNDX.LT.-1)  LUNDX = -1
         IF(LUNDX.GT. 2)  LUNDX =  2
         IF(LUNDX.GE.0) THEN
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
      WRITE ( UNIT=ERRSTR, FMT='(A,I3,A,A,I3,A)' ) &
       'BUFRLIB: OPENBF - DEGREE OF MESSAGE PRINT INDICATOR CHANGED FROM',IPRT,CPRINT(IPRT+1),' TO',LUNDX,CPRINT(LUNDX+1)
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
      CALL ERRWRT(' ')
         ENDIF
         IPRT = LUNDX
      ENDIF

      IF(IFOPBF.EQ.0) THEN

!        This is the first call to this subroutine, so take care of some
!        initial housekeeping tasks.  Note that ARALLOCF, ARALLOCC, and
!        WRDLEN must all be called prior to calling BFRINI.

!        Allocate any arrays which are being dynamically sized.
         CALL ARALLOCF
         CALL ARALLOCC

!        Figure out some important information about the local machine.
         CALL WRDLEN
       
!        Initialize some global variables.
         CALL BFRINI

         IFOPBF = 1
      ENDIF

      IF(IO.EQ.'FIRST') GOTO 100
      IF(IO.EQ.'QUIET') GOTO 100

!  See if a file can be opened
!  ---------------------------

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(LUN.EQ.0) GOTO 900
      IF(IL .NE.0) GOTO 901
      NULL(LUN) = 0
      ISC3(LUN) = 0
      ISCODES(LUN) = 0
      LUS(LUN) = 0

!  Use INQUIRE to obtain the filename associated with unit LUNIT
!  -------------------------------------------------------------

      IF (IO.NE.'NUL' .AND. IO.NE.'INUL') THEN
         INQUIRE(LUNIT,ACCESS=FILEACC)
         IF(FILEACC=='UNDEFINED') OPEN(LUNIT)
         INQUIRE(LUNIT,NAME=FILENAME)
         FILENAME=TRIM(FILENAME)//CHAR(0)
      ENDIF

!  Set initial open defaults (clear out a msg control word partition)
!  ------------------------------------------------------------------

      NMSG (LUN) = 0
      NSUB (LUN) = 0
      MSUB (LUN) = 0
      INODE(LUN) = 0
      IDATE(LUN) = 0

!  Decide how to open the file and setup the dictionary
!  ----------------------------------------------------

      IF(IO.EQ.'IN') THEN
         CALL OPENRB(LUN,FILENAME)
         CALL WTSTAT(LUNIT,LUN,-1,0)
         CALL READDX(LUNIT,LUN,LUNDX)
      ELSE IF(IO.EQ.'INUL') THEN
         CALL WTSTAT(LUNIT,LUN,-1,0)
         IF(LUNIT.NE.LUNDX) CALL READDX(LUNIT,LUN,LUNDX)
         NULL(LUN) = 1
      ELSE IF(IO.EQ.'NUL') THEN
         CALL WTSTAT(LUNIT,LUN, 1,0)
         IF(LUNIT.NE.LUNDX) CALL READDX(LUNIT,LUN,LUNDX)
         NULL(LUN) = 1
      ELSE IF(IO.EQ.'INX') THEN
         CALL OPENRB(LUN,FILENAME)
         CALL WTSTAT(LUNIT,LUN,-1,0)
         NULL(LUN) = 1
      ELSE IF(IO.EQ.'OUX') THEN
         CALL OPENWB(LUN,FILENAME)
         CALL WTSTAT(LUNIT,LUN, 1,0)
      ELSE IF(IO.EQ.'SEC3') THEN
         CALL OPENRB(LUN,FILENAME)
         CALL WTSTAT(LUNIT,LUN,-1,0)
         ISC3(LUN) = 1
      ELSE IF(IO.EQ.'OUT') THEN
         CALL OPENWB(LUN,FILENAME)
         CALL WTSTAT(LUNIT,LUN, 1,0)
         CALL WRITDX(LUNIT,LUN,LUNDX)
      ELSE IF(IO.EQ.'NODX') THEN
         CALL OPENWB(LUN,FILENAME)
         CALL WTSTAT(LUNIT,LUN, 1,0)
         CALL READDX(LUNIT,LUN,LUNDX)
      ELSE IF(IO.EQ.'APN' .OR. IO.EQ.'APX') THEN
         CALL OPENAB(LUN,FILENAME)
         CALL WTSTAT(LUNIT,LUN, 1,0)
         IF(LUNIT.NE.LUNDX) CALL READDX(LUNIT,LUN,LUNDX)
         CALL POSAPX(LUNIT)
      ELSE
         GOTO 904
      ENDIF

      GOTO 100

!     File opened for input is empty - let READMG or READERME give the bad news later

200   REWIND LUNIT
      IF(IPRT.GE.0) THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      WRITE ( UNIT=ERRSTR, FMT='(A,I3,A)' ) 'BUFRLIB: OPENBF - INPUT BUFR FILE IN UNIT ', LUNIT, ' IS EMPTY'
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
      ENDIF
      CALL WTSTAT(LUNIT,LUN,-1,0)

!  Initialize the dictionary table partition
!  -----------------------------------------

      CALL DXINIT(LUN,0)

!  Exits
!  -----

100   RETURN
900   WRITE(BORT_STR,'("BUFRLIB: OPENBF - THERE ARE ALREADY",I3," BUFR FILES OPENED, CANNOT OPEN FILE CONNECTED TO UNIT",I4)') &
                NFILES,LUNIT
      CALL BORT(BORT_STR)
901   WRITE(BORT_STR,'("BUFRLIB: OPENBF - THE FILE CONNECTED TO UNIT",I5," IS ALREADY OPEN")') LUNIT
      CALL BORT(BORT_STR)
904   CALL BORT('BUFRLIB: OPENBF - SECOND (INPUT) ARGUMENT MUST BE "IN", "OUT", "NODX", "NUL", "APN", "APX", "SEC3" OR "QUIET"')

    end subroutine openbf_body

end module
