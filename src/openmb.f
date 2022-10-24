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
C> <p>This subroutine is similar to subroutine openmg(), except that it
C> will only open a new message if either SUBSET or JDATE has changed
C> since the previous call to this subroutine.  Otherwise, it will
C> leave the existing internal message unchanged so that the next data
C> subset can be written into the same internal message, thereby
C> improving overall storage efficiency by allowing the maximum number
C> of data subsets to be stored within each output BUFR message.  For
C> this reason, openmb() is much more widely used than openmg().
C>
C> <p>If this subroutine does need to open and initialize a new BUFR
C> message for output (e.g. if the value of SUBSET or JDATE has changed
C> since the previous call to this subroutine), then any existing
C> message within the internal arrays will be automatically flushed and
C> written to logical unit LUNIT via an internal call to subroutine
C> closmg().  In this case, the behavior of this subroutine then
C> becomes exactly like that of subroutine openmg().
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
C> | 2022-08-04 | J. Woollen | Added 8-byte wrapper |

      RECURSIVE SUBROUTINE OPENMB(LUNIT,SUBSET,JDATE)

      USE MODA_MSGCWD
      USE MODV_IM8B

      CHARACTER*(*) SUBSET
      LOGICAL       OPEN

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK FOR I8 INTEGERS
C  ---------------------

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84(LUNIT,MY_LUNIT,1)
         CALL X84(JDATE,MY_JDATE,1)
         CALL OPENMB(MY_LUNIT,SUBSET,MY_JDATE)

         IM8B=.TRUE.
         RETURN
      ENDIF

C  CHECK THE FILE STATUS
C  ---------------------

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IL.LT.0) GOTO 901

C  GET SOME SUBSET PARTICULARS
C  ---------------------------

c  .... Given SUBSET, returns MTYP,MSTB,INOD
      CALL NEMTBA(LUN,SUBSET,MTYP,MSTB,INOD)
      OPEN = IM.EQ.0.OR.INOD.NE.INODE(LUN).OR.I4DY(JDATE).NE.IDATE(LUN)

C  MAYBE(?) OPEN A NEW OR DIFFERENT TYPE OF MESSAGE
C  ------------------------------------------------

      IF(OPEN) THEN
         CALL CLOSMG(LUNIT)
         CALL WTSTAT(LUNIT,LUN,IL, 1)
c  .... Set pos. index for new Tbl A mnem.
         INODE(LUN) = INOD
c  .... Set date for new message
         IDATE(LUN) = I4DY(JDATE)

C  INITIALIZE THE OPEN MESSAGE
C  ---------------------------

         CALL MSGINI(LUN)
         CALL USRTPL(LUN,1,1)
      ENDIF

C  EXITS
C  -----

      RETURN
900   CALL BORT('BUFRLIB: OPENMB - OUTPUT BUFR FILE IS CLOSED, IT '//
     . 'MUST BE OPEN FOR OUTPUT')
901   CALL BORT('BUFRLIB: OPENMB - OUTPUT BUFR FILE IS OPEN FOR '//
     . 'INPUT, IT MUST BE OPEN FOR OUTPUT')
      END
