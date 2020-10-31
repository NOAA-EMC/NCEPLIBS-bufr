C> @file
C> @authors Jack Woollen
C> @authors Jeff Ator
C> @authors Dennis Keyser
C> @date 2015-03-03

C> @brief This subroutine identifies a new logical unit to the BUFR archive
C>   library software for input or output operations.

C> However, the first time it is called, it also figures out some important
C> information about the local machine on which the software is being run,
C> and it also initializes arrays in many BUFR archive library COMMON blocks.
C> Multiple logical units can be connected to the software at any one time.
C>
C> If IO is passed in as 'QUIET', then this subroutine performs only one
C> function - it simply sets the "degree of printout" switch IPRT (in
C> COMMON block /QUIET/) to the value of input argument LUNDX, overriding
C> its previous value.  A default IPRT value of 0 (i.e. limited printout) is
C> set during the first call to this subroutine, but this or any other IPRT
C> value may be set and reset as often as desired via successive calls with
C> IO = 'QUIET'.  In all such cases, the subroutine will simply (re)set IPRT
C> and then return without actually opening any files.  The degree of
C> printout increases as IPRT increases from "-1" to "0" to "1" to "2".
C>
C> @param[in] LUNIT    - integer: FORTRAN logical unit number for BUFR
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
c                              missing value or output block type
C> @param[in] LUNDX    - integer:
C>                 - If IO is not 'QUIET' = FORTRAN logical unit number
C>                   containing dictionary table information to be used in
C>                   reading/writing from/to LUNIT (depending on the case).
C>                   This value may be set equal to LUNIT if dictionary table
C>                   information is already embedded in LUNIT
C>                 - IF IO IS 'QUIET' = indicator for degree of printout:
C>                      - -1 = no printout except for ABORT messages
C>                      -  0 = limited printout (default)
C>                      -  1 = all warning messages are printed out
C>                      -  2 = all warning and informational messages are
C>                             printed out
C>
C> PROGRAM HISTORY LOG:
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
C> THIS ROUTINE CALLS:        arallocc() arallocf() bfrini()   bort()
C>                            dxinit()   errwrt()   posapx()   readdx()
C>                            status()   wrdlen()   writdx()   wtstat()
C>                            openrb()   openwb()   openab()
C>
C> THIS ROUTINE IS CALLED BY: copybf()   getbmiss() igetmxby() mesgbc()
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

      INCLUDE 'bufrlib.prm'

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
