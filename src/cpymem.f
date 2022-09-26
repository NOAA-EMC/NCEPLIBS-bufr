C> @file
C> @brief Copy a BUFR message.

C> This subroutine copies a BUFR message from internal arrays in
C> memory to a specified Fortran logical unit.
C>
C> <p>This subroutine is similar to subroutine copymg(), except that
C> it copies a BUFR message from internal arrays in memory to a
C> specified Fortran logical unit, whereas copymg() copies a BUFR
C> message from one Fortran logical unit to another.
C>
C> @author J. Woollen
C> @date 1994-01-06
C>
C> @param[in] LUNOT   -- integer: Fortran logical unit number for
C>                       target BUFR file
C>
C> <p>One or more files of BUFR messages should have already been
C> read into internal arrays in memory via one or more previous
C> calls to subroutine ufbmem(), and a BUFR message should already
C> be in scope for processing from these arrays via a previous call
C> to subroutine rdmemm() or readmm().
C>
C> <p>Logical unit LUNOT should have already been opened for output
C> operations via a previous call to subroutine openbf(), but there
C> should not be any BUFR message already open for output within the
C> internal arrays for LUNOT via a previous call to one of the BUFRLIB
C> [message-writing subroutines](@ref hierarchy).
C>
C> <p>The [DX BUFR Table information](@ref dfbftab) associated with
C> the internal arrays in memory and with logical unit LUNOT must
C> contain identical definitions for the type of BUFR message to be
C> copied from the former to the latter.
C>
C> @remarks
C> - This subroutine uses subroutine msgwrt() to write to LUNOT;
C> therefore, it can be used to transform a copy of the
C> original BUFR message from memory with any or all of the updates
C> described in the documentation for subroutine msgwrt().
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 1994-01-06 | J. Woollen | Original author |
C> | 1998-07-08 | J. Woollen | Replaced call to Cray library routine ABORT with call to new internal routine bort() |
C> | 1999-11-18 | J. Woollen | The number of BUFR files which can be opened at one time increased from 10 to 32 (necessary in order to process multiple BUFR files under the MPI) |
C> | 2000-09-19 | J. Woollen | Maximum message length increased from 10,000 to 20,000 bytes |
C> | 2001-08-15 | D. Keyser  | Increased MAXMEM from 8 Mb to 16 Mb |
C> | 2004-08-09 | J. Ator    | Maximum message length increased from 20,000 to 50,000 bytes |
C> | 2004-11-15 | D. Keyser  | Increased MAXMEM from 16 Mb to 50 Mb |
C> | 2005-11-29 | J. Ator    | Use iupbs01() |
C> | 2009-06-26 | J. Ator    | Use iok2cpy() |
C> | 2014-12-10 | J. Ator    | Use modules instead of COMMON blocks |
C> | 2022-08-04 | J. Woollen | Added 8-byte wrapper |

      SUBROUTINE CPYMEM(LUNOT)

      USE MODA_MSGCWD
      USE MODA_BITBUF
      USE MODA_MSGMEM
      USE MODA_TABLES
      USE MODV_IM8B

      CHARACTER*8  SUBSET

      INTEGER*8 LUNOT_8

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK FOR I8 INTEGERS
C  ---------------------

      IF(IM8B) THEN
         IM8B=.FALSE.
         LUNOT_8=LUNOT
         CALL CPYMEM_8(LUNOT_8)
         IM8B=.TRUE.
         RETURN
      ENDIF

C  CHECK THE FILE STATUSES
C  -----------------------

      CALL STATUS(MUNIT,LIN,IL,IM)
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
900   CALL BORT('BUFRLIB: CPYMEM - LOGICAL UNIT NO. ASSOC. WITH INPUT'//
     . ' BUFR MESSAGES IN INTERNAL MEMORY IS CLOSED, IT MUST BE OPEN '//
     . 'FOR INPUT')
901   CALL BORT('BUFRLIB: CPYMEM - LOGICAL UNIT NO. ASSOC. WITH INPUT'//
     . ' BUFR MESSAGES IN INTERNAL MEMORY OPEN FOR OUTPUT, MUST BE '//
     . ' OPEN FOR INPUT')
902   CALL BORT('BUFRLIB: CPYMEM - A MESSAGE MUST BE OPEN IN INPUT '//
     . 'BUFR MESSAGES IN INTERNAL MEMORY, NONE ARE')
903   CALL BORT('BUFRLIB: CPYMEM - OUTPUT BUFR FILE IS CLOSED, IT '//
     . 'MUST BE OPEN FOR OUTPUT')
904   CALL BORT('BUFRLIB: CPYMEM - OUTPUT BUFR FILE IS OPEN FOR '//
     . 'INPUT, IT MUST BE OPEN FOR OUTPUT')
905   CALL BORT('BUFRLIB: CPYMEM - ALL MESSAGES MUST BE CLOSED IN '//
     . 'OUTPUT BUFR FILE, A MESSAGE IS OPEN')
906   CALL BORT('BUFRLIB: CPYMEM - INPUT BUFR MESSAGES IN INTERNAL '//
     . 'MEMORY AND OUTPUT BUFR FILE MUST HAVE SAME INTERNAL TABLES '//
     . '(DIFFERENT HERE)')

      END

C> This subroutine is an internal wrapper for handling 8-byte integer
C> arguments to subroutine cpymem().
C>
C> <p>Application programs which use 8-byte integer arguments should
C> never call this subroutine directly; instead, such programs should
C> make an initial call to subroutine setim8b() with int8b=.TRUE. and
C> then call subroutine cpymem() directly.
C>
C> @author J. Woollen
C> @date 2022-08-04
C>
C> @param[in] LUNOT_8 -- integer*8: Fortran logical unit number for
C>                       target BUFR file
C>
C> <b>Program history log:</b>
C> | Date       | Programmer | Comments             |
C> | -----------|------------|----------------------|
C> | 2022-08-04 | J. Woollen | Original author      |

      SUBROUTINE CPYMEM_8(LUNOT_8)

      INTEGER*8 LUNOT_8

      LUNOT=LUNOT_8
      CALL CPYMEM(LUNOT)

      RETURN
      END
