C> @file
C> @brief Open a new message for output in a BUFR file that was
C> previously opened for writing.

C> This subroutine opens and initializes a new BUFR message within
C> internal arrays, for eventual output to logical unit LUNIT.
C>
C> @author J. Woollen
C> @date 1994-01-06
C>
C> @param[in] LUNIT  - integer: Fortran logical unit number for BUFR
C>                     file
C> @param[in] SUBSET  - character*(*): Table A mnemonic for type of
C>                      BUFR message to be opened
C>                     (see [DX BUFR Tables](@ref dfbftab) for
C>                      further information about Table A mnemonics)
C> @param[in] JDATE  - integer: Date-time to be stored within Section 1
C>                of BUFR message being opened, in format of either
C>                YYMMDDHH or YYYYMMDDHH
C>
C> <p>Logical unit LUNIT should have already been opened for output
C> operations via a previous call to subroutine openbf().
C>
C> <p>This subroutine is similar to subroutine openmb(), except that it
C> will always open a new message for output, regardless of the values
C> of SUBSET and JDATE.  Any existing message within the internal
C> arrays will be automatically flushed and written to logical unit LUNIT
C> via an internal call to subroutine closmg().
C>
C> <b>Program history log:</b>
C> - 1994-01-06  J. Woollen -- Original author
C> - 1998-07-08  J. Woollen -- Replaced call to Cray library routine
C>                           "ABORT" with call to new internal BUFRLIB
C>                           routine "BORT"; modified to make Y2K
C>                           compliant
C> - 1999-11-18  J. Woollen -- The number of BUFR files which can be
C>                           opened at one time increased from 10 to 32
C>                           (necessary in order to process multiple
C>                           BUFR files under the MPI)
C> - 2003-11-04  J. Ator    -- Added documentation
C> - 2003-11-04  S. Bender  -- Added remarks and routine interdependencies
C> - 2003-11-04  D. Keyser  -- Unified/portable for WRF; added history
C>                           documentation; outputs more complete
C>                           diagnostic info when routine terminates
C>                           abnormally
C> - 2014-12-10  J. Ator    -- Use modules instead of COMMON blocks
C>
C> <b>This routine calls</b>: bort()     closmg()   i4dy()     msgini()
C>                            nemtba()   status()   usrtpl()   wtstat()
C>
C> <b>This routine is called by:</b> None
C>                 <br>Normally called only by application programs.
C>
      SUBROUTINE OPENMG(LUNIT,SUBSET,JDATE)

      USE MODA_MSGCWD

      INCLUDE 'bufrlib.inc'

      CHARACTER*(*) SUBSET

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK THE FILE STATUS
C  ---------------------

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IL.LT.0) GOTO 901
      IF(IM.NE.0) CALL CLOSMG(LUNIT)
      CALL WTSTAT(LUNIT,LUN,IL, 1)

C  GET SOME SUBSET PARTICULARS
C  ---------------------------

c  .... Given SUBSET, returns MTYP,MSTB,INOD
      CALL NEMTBA(LUN,SUBSET,MTYP,MSTB,INOD)
c  .... Set pos. index for new Tbl A mnem.
      INODE(LUN) = INOD
c  .... Set date for new message
      IDATE(LUN) = I4DY(JDATE)

C  INITIALIZE THE OPEN MESSAGE
C  ---------------------------

      CALL MSGINI(LUN)
      CALL USRTPL(LUN,1,1)

C  EXITS
C  -----

      RETURN
900   CALL BORT('BUFRLIB: OPENMG - OUTPUT BUFR FILE IS CLOSED, IT '//
     . 'MUST BE OPEN FOR OUTPUT')
901   CALL BORT('BUFRLIB: OPENMG - OUTPUT BUFR FILE IS OPEN FOR '//
     . 'INPUT, IT MUST BE OPEN FOR OUTPUT')
      END
