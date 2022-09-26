C> @file
C> @brief Open a new message for output in a BUFR file that was
C> previously opened for writing.

C> This subroutine opens and initializes a new BUFR message within
C> internal arrays, for eventual output to logical unit LUNIT.
C>
C> @author J. Woollen
C> @date 1994-01-06
C>
C> @param[in] LUNIT -- integer: Fortran logical unit number for BUFR
C>                     file
C> @param[in] SUBSET -- character*(*): Table A mnemonic for type of
C>                      BUFR message to be opened
C>                      (see [DX BUFR Tables](@ref dfbftab) for
C>                      further information about Table A mnemonics)
C> @param[in] JDATE -- integer: Date-time to be stored within Section 1
C>                     of BUFR message being opened, in format of either
C>                     YYMMDDHH or YYYYMMDDHH
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
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 1994-01-06 | J. Woollen | Original author |
C> | 1998-07-08 | J. Woollen | Replaced call to Cray library routine "ABORT" with call to new internal routine bort(); modified to make Y2K compliant |
C> | 1999-11-18 | J. Woollen | The number of BUFR files which can be opened at one time increased from 10 to 32 |
C> | 2003-11-04 | J. Ator    | Added documentation |
C> | 2003-11-04 | S. Bender  | Added remarks and routine interdependencies |
C> | 2003-11-04 | D. Keyser  | Unified/portable for WRF; added documentation; outputs more complete diagnostic info when routine terminates abnormally |
C> | 2014-12-10 | J. Ator    | Use modules instead of COMMON blocks |

      SUBROUTINE OPENMG(LUNIT,SUBSET,JDATE)

      USE MODA_MSGCWD
      USE MODV_IM8B

      CHARACTER*(*) SUBSET
      INTEGER*8 LUNIT_8, JDATE_8

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK FOR I8 INTEGERS
C  ---------------------

      IF(IM8B) THEN
         IM8B=.FALSE.

         LUNIT_8=LUNIT
         JDATE_8=JDATE
         CALL OPENMG_8(LUNIT_8,SUBSET,JDATE_8)

         IM8B=.TRUE.
         RETURN
      ENDIF

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

C> This subroutine is an internal wrapper for handling 8-byte integer
C> arguments to subroutine openmg().
C>
C> <p>Application programs which use 8-byte integer arguments should
C> never call this subroutine directly; instead, such programs should
C> make an initial call to subroutine setim8b() with int8b=.TRUE. and
C> then call subroutine openmg() directly.
C>
C> @author J. Woollen
C> @date 2022-08-04
C>
C> @param[in] LUNIT_8 -- integer*8: Fortran logical unit number for
C>                       BUFR file
C> @param[in] SUBSET -- character*(*): Table A mnemonic for type of
C>                      BUFR message to be opened
C> @param[in] JDATE_8 -- integer*8: Date-time to be stored within
C>                       Section 1 of BUFR message being opened
C>
C> <b>Program history log:</b>
C> | Date       | Programmer | Comments             |
C> | -----------|------------|----------------------|
C> | 2022-08-04 | J. Woollen | Original author      |

      SUBROUTINE OPENMG_8(LUNIT_8,SUBSET,JDATE_8)

      CHARACTER*(*) SUBSET
      INTEGER*8 LUNIT_8, JDATE_8

      LUNIT=LUNIT_8
      JDATE=JDATE_8
      CALL OPENMG(LUNIT,SUBSET,JDATE)

      RETURN
      END
