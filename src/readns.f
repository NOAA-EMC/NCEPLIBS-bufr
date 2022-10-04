C> @file
C> @brief Read the next data subset from a BUFR file that was
C> previously opened for reading.

C> This subroutine provides a handy way to combine the functionality
C> of subroutines readmg() and readsb() within a single subroutine
C> call.
C>
C> @author J. Woollen
C> @date 1994-01-06
C>
C> @param[in] LUNIT   -- integer: Fortran logical unit number for
C>                       BUFR file
C> @param[out] SUBSET  -- character*8: Table A mnemonic for type of
C>                        data subset that was read
C>                        (see [DX BUFR Tables](@ref dfbftab)
C>                        for further information about Table A mnemonics)
C> @param[out] JDATE   -- integer: Date-time stored within Section 1 of
C>                        BUFR message containing data subset that
C>                        was read, in format of either
C>                        YYMMDDHH or YYYYMMDDHH, depending on the most
C>                        recent call to subroutine datelen()
C> @param[out] IRET    -- integer: return code
C>                           - 0 = new BUFR data subset was successfully
C>                                 read into internal arrays
C>                           - -1 = there are no more BUFR data subsets
C>                                 in the file connected to logical unit
C>                                 LUNIT
C>
C> <p>Logical unit LUNIT should have already been opened for
C> input operations via a previous call to subroutine openbf().
C> But once that is done, the application program can immediately call
C> this subroutine to read each new data subset from the
C> associated BUFR file, and the subroutine will automatically open
C> and close each new BUFR message internally as needed, so that
C> subsequent calls can immediately be made to any of the various
C> [values-reading subroutines](@ref hierarchy).
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 1994-01-06 | J. Woollen | Original author |
C> | 1998-07-08 | J. Woollen | Replaced call to Cray library routine "ABORT" with call to new internal routine bort(); modified to make Y2K compliant |
C> | 1999-11-18 | J. Woollen | The number of BUFR files which can be opened at one time increased from 10 to 32 |
C> | 2003-11-04 | D. Keyser  | MAXJL (maximum number of jump/link entries) increased from 15000 to 16000 |
C> | 2014-12-10 | J. Ator    | Use modules instead of COMMON blocks |
C> | 2022-10-04 | J. Ator    | Added 8-byte wrapper |

      SUBROUTINE READNS(LUNIT,SUBSET,JDATE,IRET)

      USE MODA_MSGCWD
      USE MODA_TABLES
      USE MODV_IM8B

      CHARACTER*8  SUBSET

      INTEGER*8 LUNIT_8,JDATE_8,IRET_8

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK FOR I8 INTEGERS
C  ---------------------

      IF(IM8B) THEN
         IM8B=.FALSE.

         LUNIT_8=LUNIT
         CALL READNS_8(LUNIT_8,SUBSET,JDATE_8,IRET_8)
         JDATE=JDATE_8
         IRET=IRET_8

         IM8B=.TRUE.
         RETURN
      ENDIF

C  REFRESH THE SUBSET AND JDATE PARAMETERS
C  ---------------------------------------

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IL.GT.0) GOTO 901
      SUBSET = TAG(INODE(LUN))
      JDATE  = IDATE(LUN)

C  READ THE NEXT SUBSET IN THE BUFR FILE
C  -------------------------------------

1     CALL READSB(LUNIT,IRET)
      IF(IRET.NE.0) THEN
         CALL READMG(LUNIT,SUBSET,JDATE,IRET)
         IF(IRET.EQ.0) GOTO 1
      ENDIF

C  EXITS
C  -----

      RETURN
900   CALL BORT('BUFRLIB: READNS - INPUT BUFR FILE IS CLOSED, IT MUST'//
     . ' BE OPEN FOR INPUT')
901   CALL BORT('BUFRLIB: READNS - INPUT BUFR FILE IS OPEN FOR OUTPUT'//
     . ', IT MUST BE OPEN FOR INPUT')
      END

C> This subroutine is an internal wrapper for handling 8-byte integer
C> arguments to subroutine readns().
C>
C> <p>Application programs which use 8-byte integer arguments should
C> never call this subroutine directly; instead, such programs should
C> make an initial call to subroutine setim8b() with int8b=.TRUE. and
C> then call subroutine readns() directly.
C>
C> @author J. Ator
C> @date 2022-10-04
C>
C> @param[in] LUNIT_8 -- integer*8: Fortran logical unit number for
C>                       BUFR file
C> @param[out] SUBSET  -- character*8: Table A mnemonic for type of BUFR
C>                        message that was read
C> @param[out] JDATE_8 -- integer*8: Date-time stored within Section 1 of
C>                        BUFR message that was read
C> @param[out] IRET_8  -- integer*8: return code
C>
C> <b>Program history log:</b>
C> | Date       | Programmer | Comments             |
C> | -----------|------------|----------------------|
C> | 2022-10-04 | J. Ator    | Original author      |

      SUBROUTINE READNS_8(LUNIT_8,SUBSET,JDATE_8,IRET_8)

      CHARACTER*8 SUBSET
      INTEGER*8 LUNIT_8,JDATE_8,IRET_8

      LUNIT=LUNIT_8
      CALL READNS(LUNIT,SUBSET,JDATE,IRET)
      JDATE_8=JDATE
      IRET_8=IRET

      RETURN
      END
