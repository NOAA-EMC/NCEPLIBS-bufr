C> @file
C> @brief Identify a new system file to the BUFRLIB software for
C> reading or writing BUFR messages.

C> @authors J. Woollen
C> @authors J. Ator
C> @authors D. Keyser
C> @date 1994-01-06
C>
C> This subroutine identifies a new file to the BUFRLIB software for
C> input or output operations.
C>
C> @param[in] LUNIT    - integer: Fortran logical unit number for BUFR
C>                       file (unless IO is set to 'QUIET', in which
C>                       case this is then a dummy argument)
C> @param[in] IO       - character*(*): flag indicating how LUNIT is to be
C>                       used by the software:
C>                 -   'IN' = input operations with table processing
C>                 -   'INX' = input operations w/o table processing
C>                 -   'OUX' = output operations w/o table processing
C>                 -   'OUT' = output operations with table processing
C>                 -  'SEC3' = same as 'IN', except use Section 3 of input 
C>                              messages for decoding rather than dictionary
C>                              table information from LUNDX; in this case
C>                              LUNDX is ignored, and user must provide 
C>                              appropriate BUFR master tables within
C>                              directory specified by a subsequent call
C>                              to subroutine mtinfo()
C>                 -  'NODX' = same as 'OUT', except don't write dictionary
C>                             (i.e. DX) table messages to LUNIT
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
C>                             /QUIET/ is being reset (see LUNDX)
C>                 - 'FIRST' = calls bfrini() and wrdlen() as a prelude to
C>                             user resetting of global parameters such as
C>                             missing value or output block type
C> @param[in] LUNDX    - integer:
C>                 - If IO is not 'QUIET' = Fortran logical unit number
C>                   containing dictionary table information to be used in
C>                   reading/writing from/to LUNIT (depending on the case).
C>                   This value may be set equal to LUNIT if dictionary table
C>                   information is already embedded in LUNIT
C>                 - If IO is 'QUIET' = indicator for degree of printout:
C>                      - -1 = no printout except for ABORT messages
C>                      -  0 = limited printout (default)
C>                      -  1 = all warning messages are printed out
C>                      -  2 = all warning and informational messages are
C>                             printed out
C>
C> <p>The logical unit numbers LUNIT and LUNDX must already be associated
C> with actual filenames on the local system, typically via a Fortran "OPEN"
C> statement. Multiple logical units can be connected to the BUFRLIB software
C> at any one time.
C>
C> <p>The argument IO is a character string describing how the file connected to
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
C> The only exception is when IO = 'QUIET', and in this case the subroutine
C> performs only one function - it simply sets or resets the print verbosity
C> switch to the value of input argument LUNDX, overriding its previous value.
C> A default verbosity value of 0 (i.e. limited printout) is set during the first
C> call to this subroutine, but other options are also available as shown below.
C>
C> <p>The third and final call argument LUNDX identifies the logical unit which
C> contains the definition of the DX BUFR tables to be associated with unit
C> LUNIT.  Except when IO = 'SEC3', every BUFR file that is linked to the BUFRLIB
C> software must have a DX BUFR tables file associated with it, and these tables
C> may be defined within a separate ASCII text file (see Description and Format
C> of DX BUFR Tables for more info.) or, in the case of an existing BUFR file,
C> may be embedded within the first few BUFR messages of the file itself, and in
C> which case the user can denote this to the subroutine by setting LUNDX to the
C> same value as LUBFR.
C>
C> @remarks
C> - When an existing BUFR file is accessed for input (i.e. reading/decoding BUFR),
C> the associated DX BUFR tables defined by LUNDX are stored internally within
C> the BUFRLIB software and are referenced during all subsequent processing of
C> the file. Likewise, when a file is accessed for output (i.e. writing/encoding
C> BUFR), the associated DX BUFR tables are still stored internally for subsequent
C> reference; however, the output file itself is also initialized by writing the
C> BUFR table information (as one or more BUFR messages) to the beginning of the
C> file, except when IO = 'NODX', and in which case the writing of these
C> additional messages is suppressed.
C> - As noted above, 'SEC3' is the only value of IO (other than 'QUIET') where it's
C> not necessary to provide pre-defined DX BUFR tables via LUNDX.  Instead, this
C> option instructs the BUFRLIB software to unpack the data description section
C> (Section 3) from each BUFR message it reads and then decode the contents
C> accordingly. In this case, it's necessary to provide a set of BUFR master
C> tables containing listings of all possible BUFR descriptors (see Description
C> and Format of BUFR Master Tables for more info.), but otherwise no prior
C> knowledge is required of the contents of the messages to be decoded.
C>
C> <b>Program history log:</b>
C> - 1994-01-06  J. Woollen -- Original author
C> - 1998-07-08  J. Woollen -- Replaced call to Cray library routine ABORT
C>                             with call to new internal routine bort()
C> - 1999-11-18  J. Woollen -- The number of BUFR files which can be
C>                             opened at one time increased from 10 to 32
C>                             (necessary in order to process multiple
C>                             BUFR files under the MPI)
C> - 2003-11-04  J. Ator    -- Added IO='NUL' option in order to prevent
C>                             later writing to BUFR file in LUNIT (was in
C>                             decoder version); added documentation
C> - 2003-11-04  S. Bender  -- Added remarks and routine interdependencies
C> - 2003-11-04  D. Keyser  -- Unified/portable for WRF; added history
C>                             documentation; outputs more complete
C>                             diagnostic info when routine terminates
C>                             abnormally, unusual things happen or for
C>                             informational purposes
C> - 2004-08-18  J. Ator    -- added SAVE for IFIRST flag and IO="NODX"
C>                             option 
C> - 2005-11-29  J. Ator    -- added COMMON /MSGFMT/ and ichkstr() call
C> - 2009-03-23  J. Ator    -- added IO='SEC3' option; removed call to
C>                             posapn; clarified comments; use errwrt()
C> - 2010-05-11  J. Ator    -- added COMMON /STCODE/
C> - 2012-06-18  J. Ator    -- added IO='INUL' option
C> - 2012-09-15  J. Woollen -- modified for C/I/O/BUFR interface;
C>                             use INQUIRE to obtain the filename;
C>                             call C routines openrb(), openwb() and 
C>                             openab() to connect BUFR files to C;
C>                             added IO type 'INX' to enable open and
C>                             close for C file without closing FORTRAN
C>                             file; add IO type 'FIRST' to support calls   
C>                             to bfrini() and wrdlen() prior to user reset
C>                             of BUFRLIB parameters found in new routines 
C>                             setbmiss() and setblock()
C> - 2014-11-07  J. Ator    -- allow dynamic allocation of certain arrays
C> - 2015-03-03  J. Ator    -- use MODA_IFOPBF instead of IFIRST
C>
C> <b>This routine calls:</b> arallocc() arallocf() bfrini()   bort()
C>                            dxinit()   errwrt()   posapx()   readdx()
C>                            status()   wrdlen()   writdx()   wtstat()
C>                            openrb()   openwb()   openab()
C>
C> <b>This routine is called by:</b>
C>                            copybf()   getbmiss() igetmxby() mesgbc()
C>                            mesgbf()   pkvs01()   rdmgsb()   ufbinx()
C>                            ufbmem()   ufbmex()   ufbtab() setbmiss()
C>                            setblock()
C>                            <br>Also called by application programs.
C>
      SUBROUTINE OPENBF(LUNIT,IO,LUNDX)

      USE MODA_MSGCWD
      USE MODA_STBFR
      USE MODA_SC3BFR
      USE MODA_LUSHR
      USE MODA_NULBFR
      USE MODA_STCODE
      USE MODA_IFOPBF

      INCLUDE 'bufrlib.inc'

      COMMON /QUIET / IPRT

      CHARACTER*(*) IO
      CHARACTER*255 filename,fileacc   
      CHARACTER*128 BORT_STR,ERRSTR
      CHARACTER*28  CPRINT(0:3)
      CHARACTER*1   BSTR(4)

      DATA          CPRINT/
     . ' (only ABORTs)              ',
     . ' (limited - default)        ',
     . ' (all warnings)             ',
     . ' (all warning+informational)'/

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C     If this is the first call to this subroutine, initialize
C     IPRT in /QUIET/ as 0 (limited printout - except for abort
C     messages)

      IF(IFOPBF.EQ.0) IPRT = 0

      IF(IO.EQ.'QUIET') THEN
c  .... override previous IPRT value (printout indicator)
         IF(LUNDX.LT.-1)  LUNDX = -1
         IF(LUNDX.GT. 2)  LUNDX =  2
         IF(LUNDX.GE.0) THEN
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
      WRITE ( UNIT=ERRSTR, FMT='(A,I3,A,A,I3,A)' )
     . 'BUFRLIB: OPENBF - DEGREE OF MESSAGE PRINT INDICATOR '//
     . 'CHNGED FROM',IPRT,CPRINT(IPRT+1),' TO',LUNDX,CPRINT(LUNDX+1)
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
      CALL ERRWRT(' ')
         ENDIF
         IPRT = LUNDX
      ENDIF

      IF(IFOPBF.EQ.0) THEN

C        This is the first call to this subroutine, so take care of some
C        initial housekeeping tasks.  Note that ARALLOCF, ARALLOCC, and
C        WRDLEN must all be called prior to calling BFRINI.

#ifdef DYNAMIC_ALLOCATION
C        Allocate any arrays which are being dynamically sized.
         CALL ARALLOCF
         CALL ARALLOCC
#endif

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

C     FILE OPENED FOR INPUT IS EMPTY - LET READMG OR READERME GIVE
C     THE BAD NEWS LATER

200   REWIND LUNIT
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
