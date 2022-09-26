C> @file
C> @brief Copy a BUFR message.

C> This subroutine copies a BUFR message from one Fortran logical unit
C> to another.
C>
C> <p>This subroutine is similar to subroutine cpymem(), except that
C> it copies a BUFR message from one Fortran logical unit to another,
C> whereas cpymem() copies a BUFR message from internal arrays in
C> memory to a specified Fortran logical unit.
C>
C> @author J. Woollen
C> @date 1994-01-06
C>
C> @param[in] LUNIN   -- integer: Fortran logical unit number for
C>                       source BUFR file
C> @param[in] LUNOT   -- integer: Fortran logical unit number for
C>                       target BUFR file
C>
C> <p>Logical unit LUNIN should have already been opened for input
C> operations via a previous call to subroutine openbf(), and a BUFR
C> message should have already been read into internal arrays for
C> LUNIN via a previous call to one of the
C> [message-reading subroutines](@ref hierarchy).
C>
C> <p>Logical unit LUNOT should have already been opened for output
C> operations via a previous call to subroutine openbf(), but there
C> should not be any BUFR message already open for output within the
C> internal arrays for LUNOT via a previous call to one of the BUFRLIB
C> [message-writing subroutines](@ref hierarchy).
C>
C> <p>The [DX BUFR Table information](@ref dfbftab) associated with
C> each of the logical units LUNIN and LUNOT must contain identical
C> definitions for the type of BUFR message to be copied from LUNIN
C> to LUNOT.
C>
C> @remarks
C> - This subroutine uses subroutine msgwrt() to write to LUNOT;
C> therefore, it can be used to transform a copy of the
C> original BUFR message from LUNIN with any or all of the updates
C> described in the documentation for subroutine msgwrt().
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 1994-01-06 | J. Woollen | Original author |
C> | 1998-07-08 | J. Woollen | Replaced call to Cray library routine "ABORT" with call to new internal routine bort() |
C> | 1999-11-18 | J. Woollen | The number of BUFR files which can be opened at one time increased from 10 to 32 (necessary in order to process multiple BUFR files under the MPI) |
C> | 2000-09-19 | J. Woollen | Maximum message length increased from 10,000 to 20,000 bytes |
C> | 2004-08-09 | J. Ator | Maximum message length increased from 20,000 to 50,000 bytes |
C> | 2005-11-29 | J. Ator | Use iupbs01() |
C> | 2009-06-26 | J. Ator | Use iok2cpy() |
C> | 2014-12-10 | J. Ator | Use modules instead of COMMON blocks |
C> | 2022-08-04 | J. Woollen | Added 8-byte wrapper |

      SUBROUTINE COPYMG(LUNIN,LUNOT)

      USE MODA_MSGCWD
      USE MODA_BITBUF
      USE MODA_TABLES
      USE MODV_IM8B

      CHARACTER*8  SUBSET

      INTEGER*8 LUNIN_8,LUNOT_8
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK FOR I8 INTEGERS
C  ---------------------

      IF(IM8B) THEN
         IM8B=.FALSE.

         LUNIN_8=LUNIN
         LUNOT_8=LUNOT
         CALL COPYMG_8(LUNIN_8,LUNOT_8)

         IM8B=.TRUE.  
         RETURN
      ENDIF

C  CHECK THE FILE STATUSES
C  -----------------------

      CALL STATUS(LUNIN,LIN,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IL.GT.0) GOTO 901
      IF(IM.EQ.0) GOTO 902

      CALL STATUS(LUNOT,LOT,IL,IM)
      IF(IL.EQ.0) GOTO 903
      IF(IL.LT.0) GOTO 904
      IF(IM.NE.0) GOTO 905

C  MAKE SURE BOTH FILES HAVE THE SAME TABLES
C  -----------------------------------------

      SUBSET = TAG(INODE(LIN))
c  .... Given SUBSET, returns MTYP,MSBT,INOD
      CALL NEMTBA(LOT,SUBSET,MTYP,MSBT,INOD)
      IF(INODE(LIN).NE.INOD) THEN
        IF(IOK2CPY(LIN,LOT).NE.1) GOTO 906
      ENDIF

C  EVERYTHING OKAY, COPY A MESSAGE
C  -------------------------------

      MBYM = IUPBS01(MBAY(1,LIN),'LENM')
      CALL MSGWRT(LUNOT,MBAY(1,LIN),MBYM)

C  SET THE MESSAGE CONTROL WORDS FOR PARTITION ASSOCIATED WITH LUNOT
C  -----------------------------------------------------------------

      NMSG (LOT) = NMSG(LOT) + 1
      NSUB (LOT) = MSUB(LIN)
      MSUB (LOT) = MSUB(LIN)
      IDATE(LOT) = IDATE(LIN)
      INODE(LOT) = INOD

C  EXITS
C  -----

      RETURN
900   CALL BORT('BUFRLIB: COPYMG - INPUT BUFR FILE IS CLOSED, IT MUST'//
     . ' BE OPEN FOR INPUT')
901   CALL BORT('BUFRLIB: COPYMG - INPUT BUFR FILE IS OPEN FOR '//
     . 'OUTPUT, IT MUST BE OPEN FOR INPUT')
902   CALL BORT('BUFRLIB: COPYMG - A MESSAGE MUST BE OPEN IN INPUT '//
     . 'BUFR FILE, NONE ARE')
903   CALL BORT('BUFRLIB: COPYMG - OUTPUT BUFR FILE IS CLOSED, IT '//
     . 'MUST BE OPEN FOR OUTPUT')
904   CALL BORT('BUFRLIB: COPYMG - OUTPUT BUFR FILE IS OPEN FOR '//
     . 'INPUT, IT MUST BE OPEN FOR OUTPUT')
905   CALL BORT('BUFRLIB: COPYMG - ALL MESSAGES MUST BE CLOSED IN '//
     . 'OUTPUT BUFR FILE, A MESSAGE IS OPEN')
906   CALL BORT('BUFRLIB: COPYMG - INPUT AND OUTPUT BUFR FILES MUST '//
     . 'HAVE THE SAME INTERNAL TABLES, THEY ARE DIFFERENT HERE')
      END

C> This subroutine is an internal wrapper for handling 8-byte integer
C> arguments to subroutine copymg().
C>
C> <p>Application programs which use 8-byte integer arguments should
C> never call this subroutine directly; instead, such programs should
C> make an initial call to subroutine setim8b() with int8b=.TRUE. and
C> then call subroutine copymg() directly.
C>
C> @author J. Woollen
C> @date 2022-08-04
C>
C> @param[in] LUNIN_8 -- integer*8: Fortran logical unit number for
C>                       source BUFR file
C> @param[in] LUNOT_8 -- integer*8: Fortran logical unit number for
C>                       target BUFR file
C>
C> <b>Program history log:</b>
C> | Date       | Programmer | Comments             |
C> | -----------|------------|----------------------|
C> | 2022-08-04 | J. Woollen | Original author      |

      SUBROUTINE COPYMG_8(LUNIN_8,LUNOT_8)

      INTEGER*8 LUNIN_8,LUNOT_8

      LUNIN=LUNIN_8
      LUNOT=LUNOT_8
      CALL COPYMG(LUNIN,LUNOT)

      RETURN
      END
