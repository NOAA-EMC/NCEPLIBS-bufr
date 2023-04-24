C> @file
C> @brief Connect a new file to the library, or initialize the
C> library, or change verbosity associated with already-connected file.
C>
C> @authors J. Woollen, J. Ator,  D. Keyser @date 1994-01-06

C> Connects a new file to the NCEPLIBS-bufr software for
C> input or output operations, or initializes the library without
C> connecting to a file, or changes the verbosity of the library for an
C> already-connected BUFR file.
C>
C> The logical unit numbers LUNIT and LUNDX must already be associated
C> with actual filenames on the local system, typically via a Fortran "OPEN"
C> statement. Multiple logical units can be connected to the NCEPLIBS-bufr software
C> at any one time.
C>
C> The argument IO is a character string describing how the file connected to
C> LUNIT will be used, e.g. 'IN' is used to access an existing file of BUFR
C> messages for input (i.e. reading/decoding BUFR), and 'OUT' is used to access
C> a new file for output (i.e. writing/encoding BUFR). An option 'APX' is also
C> available which behaves like 'OUT', except that output is then appended to
C> an existing BUFR file rather than creating a new one from scratch, and there
C> are also some additional options 'NUL' and 'NODX' which can likewise be used
C> instead of 'OUT' for some very special cases as needed. There's also an
C> option 'SEC3' which can be used in place of 'IN' for certain cases when the
C> user is attempting to read BUFR messages whose content and descriptor layout
C> are unknown in advance. However, all of these additional options are
C> basically just variations of 'IN' or 'OUT', again depending on whether the
C> intent is to read or write BUFR messages from the file connected to LUNIT.
C> The only exceptions are when IO = 'FIRST' or 'QUIET'.  When IO = 'FIRST',
C> the subroutine simply checks whether it has already been called from within
C> the application program and, if not, goes ahead and initializes the library
C> without actually connecting any files in LUNIT or LUNDX.
C>
C> Alternatively, when IO = 'QUIET', the subroutine simply sets or resets the
C> internal print verbosity switch to the value of input argument LUNDX,
C> overriding its previous value and/or its internal default value of 0.
C>
C> The third and final call argument LUNDX identifies the logical unit which
C> contains the definition of the DX BUFR tables to be associated with unit
C> LUNIT.  Except when IO = 'SEC3', every BUFR file that is linked to the NCEPLIBS-bufr
C> software must have a DX BUFR tables file associated with it, and these tables
C> may be defined within a separate ASCII text file
C> (see [Description and Format of DX BUFR Tables](@ref dfbftab) for more info.)
C> or, in the case of an existing BUFR file, may be embedded within the first few
C> BUFR messages of the file itself, and in which case the user can denote this
C> to the subroutine by setting LUNDX to the same value as LUBFR.
C>
C> @remarks
C> - When an existing BUFR file is accessed for input (i.e. reading/decoding BUFR),
C> the associated DX BUFR tables defined by LUNDX are stored internally within
C> the NCEPLIBS-bufr software and are referenced during all subsequent processing of
C> the file. Likewise, when a file is accessed for output (i.e. writing/encoding
C> BUFR), the associated DX BUFR tables are still stored internally for subsequent
C> reference; however, the output file itself is also initialized by writing the
C> BUFR table information (as one or more BUFR messages) to the beginning of the
C> file, except when IO = 'NODX', and in which case the writing of these
C> additional messages is suppressed.
C> - As noted above, 'SEC3' is the only value of IO (other than 'QUIET') where it's
C> not necessary to provide pre-defined DX BUFR tables via LUNDX.  Instead, this
C> option instructs the NCEPLIBS-bufr software to unpack the data description section
C> (Section 3) from each BUFR message it reads and then decode the contents
C> accordingly. In this case, it's necessary to provide a set of BUFR master
C> tables containing listings of all possible BUFR descriptors
C> (see [Description and Format of master BUFR Tables](@ref dfbfmstab) for more
C> info.), but otherwise no prior knowledge is required of the contents of the
C> messages to be decoded.
C>
C>
C> @param[in] LUNIT   -- integer: Fortran logical unit number for BUFR
C>                       file (unless IO is set to 'FIRST' or 'QUIET', in
C>                       which case this is a dummy argument)
C> @param[in] IO      -- character*(*): flag indicating how LUNIT is to be
C>                       used by the software:
C>                 -   'IN' = input operations with table processing
C>                 -   'INX' = input operations w/o table processing
C>                 -   'OUX' = output operations w/o table processing
C>                 -   'OUT' = output operations with table processing
C>                 -  'SEC3' = same as 'IN', except use Section 3 of input
C>                              messages for decoding rather than DX BUFR
C>                              table information from LUNDX; in this case
C>                              LUNDX is ignored, and user must provide
C>                              appropriate [master BUFR tables](@ref dfbfmstab)
C>                              within the directory specified by a subsequent
C>                              call to subroutine mtinfo()
C>                 -  'NODX' = same as 'OUT', except don't write DX BUFR
C>                             table messages to LUNIT
C>                 -   'APN' = same as 'NODX', except begin writing at end
C>                             of file ("append")
C>                 -   'APX' = same as 'APN', except backspace before
C>                             appending
C>                 -   'NUL' = same as 'OUT', except don't write any
C>                             messages whatsoever to LUNIT (e.g. when
C>                             subroutine writsa() is to be used)
C>                 -  'INUL' = same as 'IN', except don't read any
C>                             messages whatsoever from LUNIT (e.g. when
C>                             subroutine readerme() is to be used)
C>                 - 'QUIET' = LUNIT is ignored; this is an indicator
C>                             that the value for IPRT in COMMON block
C>                             /QUIET/ is being reset to the value in
C>                             LUNDX
C>                 - 'FIRST' = LUNIT and LUNDX are ignored; this is an
C>                             indicator to initialize the NCEPLIBS-bufr
C>                             software, in case this subroutine was
C>                             never previously called
C> @param[in] LUNDX   -- integer:
C>                 - If IO is not set to 'FIRST' or 'QUIET' =
C>                   Fortran logical unit number
C>                   containing DX BUFR table information to be used in
C>                   reading/writing from/to LUNIT (depending on the case).
C>                   This value may be set equal to LUNIT if DX BUFR table
C>                   information is already embedded in LUNIT.
C>                 - If IO is set to 'QUIET' = indicator for degree of
C>                   printout:
C>                      - -1 = no printout except for ABORT messages
C>                      -  0 = limited printout (default)
C>                      -  1 = all warning messages are printed out
C>                      -  2 = all warning and informational messages are
C>                             printed out
C>
C> @authors J. Woollen, J. Ator,  D. Keyser @date 1994-01-06

      RECURSIVE SUBROUTINE OPENBF(LUNIT,IO,LUNDX)

      use bufrlib

      USE MODV_IFOPBF
      USE MODV_NFILES
      USE MODV_IM8B

      USE MODA_MSGCWD
      USE MODA_STBFR
      USE MODA_SC3BFR
      USE MODA_LUSHR
      USE MODA_NULBFR
      USE MODA_STCODE

      COMMON /QUIET / IPRT

      CHARACTER*(*) IO
      CHARACTER*255 FILENAME,FILEACC
      CHARACTER*128 BORT_STR,ERRSTR
      CHARACTER*28  CPRINT(0:3)

      DATA          CPRINT/
     . ' (only ABORTs)              ',
     . ' (limited - default)        ',
     . ' (all warnings)             ',
     . ' (all warning+informational)'/

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK FOR I8 INTEGERS
C  ---------------------

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84(LUNIT,MY_LUNIT,1)
         CALL X84(LUNDX,MY_LUNDX,1)
         CALL OPENBF(MY_LUNIT,IO,MY_LUNDX)

         IM8B=.TRUE.
         RETURN
      ENDIF

C     If this is the first call to this subroutine, initialize
C     IPRT in /QUIET/ as 0 (limited printout - except for abort
C     messages)

      IF(IFOPBF.EQ.0) IPRT = 0

      IF(IO.EQ.'QUIET') THEN
c  .... override previous IPRT value (printout indicator)
         IPRTPRV = IPRT
         IPRT = LUNDX
         IF(IPRT.LT.-1) IPRT = -1
         IF(IPRT.GT. 2) IPRT =  2
         IF(IPRT.GE.0) THEN
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
      WRITE ( UNIT=ERRSTR, FMT='(A,I3,A,A,I3,A)' )
     . 'BUFRLIB: OPENBF - DEGREE OF MESSAGE PRINT INDICATOR '//
     . 'CHNGED FROM',IPRTPRV,CPRINT(IPRTPRV+1),' TO',IPRT,CPRINT(IPRT+1)
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
      CALL ERRWRT(' ')
         ENDIF
      ENDIF

      IF(IFOPBF.EQ.0) THEN

C        This is the first call to this subroutine, so take care of some
C        initial housekeeping tasks.  Note that ARALLOCF, ARALLOCC_C, and
C        WRDLEN must all be called prior to calling BFRINI.

C        Allocate any arrays which are being dynamically sized.
         CALL ARALLOCF
         CALL ARALLOCC_C

C        Figure out some important information about the local machine.
         CALL WRDLEN

C        Initialize some global variables.
         CALL BFRINI

         IFOPBF = 1
      ENDIF

      IF(IO.EQ.'FIRST') GOTO 100
      IF(IO.EQ.'QUIET') GOTO 100

C  SEE IF A FILE CAN BE OPENED
C  ---------------------------

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(LUN.EQ.0) GOTO 900
      IF(IL .NE.0) GOTO 901
      NULL(LUN) = 0
      ISC3(LUN) = 0
      ISCODES(LUN) = 0
      LUS(LUN) = 0

C  USE INQUIRE TO OBTAIN THE FILENAME ASSOCIATED WITH UNIT LUNIT
C  -------------------------------------------------------------

      IF (IO.NE.'NUL' .AND. IO.NE.'INUL') THEN
         INQUIRE(LUNIT,ACCESS=FILEACC)
         IF(FILEACC=='UNDEFINED') OPEN(LUNIT)
         INQUIRE(LUNIT,NAME=FILENAME)
         FILENAME=TRIM(FILENAME)//CHAR(0)
      ENDIF

C  SET INITIAL OPEN DEFAULTS (CLEAR OUT A MSG CONTROL WORD PARTITION)
C  ------------------------------------------------------------------

      NMSG (LUN) = 0
      NSUB (LUN) = 0
      MSUB (LUN) = 0
      INODE(LUN) = 0
      IDATE(LUN) = 0

C  DECIDE HOW TO OPEN THE FILE AND SETUP THE DICTIONARY
C  ----------------------------------------------------

      IF(IO.EQ.'IN') THEN
         CALL OPENRB_C(LUN,FILENAME)
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
         CALL OPENRB_C(LUN,FILENAME)
         CALL WTSTAT(LUNIT,LUN,-1,0)
         NULL(LUN) = 1
      ELSE IF(IO.EQ.'OUX') THEN
         CALL OPENWB_C(LUN,FILENAME)
         CALL WTSTAT(LUNIT,LUN, 1,0)
      ELSE IF(IO.EQ.'SEC3') THEN
         CALL OPENRB_C(LUN,FILENAME)
         CALL WTSTAT(LUNIT,LUN,-1,0)
         ISC3(LUN) = 1
      ELSE IF(IO.EQ.'OUT') THEN
         CALL OPENWB_C(LUN,FILENAME)
         CALL WTSTAT(LUNIT,LUN, 1,0)
         CALL WRITDX(LUNIT,LUN,LUNDX)
      ELSE IF(IO.EQ.'NODX') THEN
         CALL OPENWB_C(LUN,FILENAME)
         CALL WTSTAT(LUNIT,LUN, 1,0)
         CALL READDX(LUNIT,LUN,LUNDX)
      ELSE IF(IO.EQ.'APN' .OR. IO.EQ.'APX') THEN
         CALL OPENAB_C(LUN,FILENAME)
         CALL WTSTAT(LUNIT,LUN, 1,0)
         IF(LUNIT.NE.LUNDX) CALL READDX(LUNIT,LUN,LUNDX)
         CALL POSAPX(LUNIT)
      ELSE
         GOTO 904
      ENDIF

      GOTO 100

C     FILE OPENED FOR INPUT IS EMPTY - LET READMG OR READERME GIVE
C     THE BAD NEWS LATER

      REWIND LUNIT
      IF(IPRT.GE.0) THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      WRITE ( UNIT=ERRSTR, FMT='(A,I3,A)' )
     .  'BUFRLIB: OPENBF - INPUT BUFR FILE IN UNIT ', LUNIT,
     .  ' IS EMPTY'
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
      ENDIF
      CALL WTSTAT(LUNIT,LUN,-1,0)

C  INITIALIZE THE DICTIONARY TABLE PARTITION
C  -----------------------------------------

      CALL DXINIT(LUN,0)

C  EXITS
C  -----

100   RETURN
900   WRITE(BORT_STR,'("BUFRLIB: OPENBF - THERE ARE ALREADY",I3,'//
     . '" BUFR FILES OPENED, CANNOT OPEN FILE CONNECTED TO UNIT",I4)')
     . NFILES,LUNIT
      CALL BORT(BORT_STR)
901   WRITE(BORT_STR,'("BUFRLIB: OPENBF - THE FILE CONNECTED TO UNIT"'//
     . ',I5," IS ALREADY OPEN")') LUNIT
      CALL BORT(BORT_STR)
904   CALL BORT('BUFRLIB: OPENBF - SECOND (INPUT) ARGUMENT MUST BE'//
     . ' "IN", "OUT", "NODX", "NUL", "APN", "APX", "SEC3"'//
     . ' OR "QUIET"')
      END
