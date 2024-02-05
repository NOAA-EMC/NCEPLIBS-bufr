C> @file
C> @brief Copy a BUFR message.
C>
C> @author J. Woollen @date 1994-01-06

C> Copy a BUFR message from internal arrays to a file.
C>
C> This subroutine is similar to subroutine copymg(), except that
C> it copies a BUFR message from internal arrays in memory to a
C> specified Fortran logical unit, whereas copymg() copies a BUFR
C> message from one Fortran logical unit to another.
C>
C> One or more files of BUFR messages should have already been
C> read into internal arrays in memory via one or more previous
C> calls to subroutine ufbmem(), and a BUFR message should already
C> be in scope for processing from these arrays via a previous call
C> to subroutine rdmemm() or readmm().
C>
C> Logical unit LUNOT should have already been opened for output
C> operations via a previous call to subroutine openbf(), but there
C> should not be any BUFR message already open for output within the
C> internal arrays for LUNOT via a previous call to one of the
C> [message-writing subroutines](@ref hierarchy).
C>
C> The [DX BUFR Table information](@ref dfbftab) associated with
C> the internal arrays in memory and with logical unit LUNOT must
C> contain identical definitions for the type of BUFR message to be
C> copied from the former to the latter.
C>
C> This subroutine uses subroutine msgwrt() to write to LUNOT;
C> therefore, it can be used to transform a copy of the
C> original BUFR message from memory with any or all of the updates
C> described in the documentation for subroutine msgwrt().
C>
C> @param[in] LUNOT - integer: Fortran logical unit number for
C> target BUFR file.
C>
C> @author J. Woollen @date 1994-01-06
      RECURSIVE SUBROUTINE CPYMEM(LUNOT)

      use modv_vars, only: im8b

      use moda_msgcwd
      use moda_bitbuf
      use moda_msgmem
      use moda_tables

      CHARACTER*8  SUBSET

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK FOR I8 INTEGERS
C  ---------------------

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84(LUNOT,MY_LUNOT,1)
         CALL CPYMEM(MY_LUNOT)

         IM8B=.TRUE.
         RETURN
      ENDIF

C  CHECK THE FILE STATUSES
C  -----------------------

      CALL STATUS(MUNIT,LIN,IL,IM)
      IF(IM.EQ.0) GOTO 902

      CALL STATUS(LUNOT,LOT,IL,IM)
      IF(IL.EQ.0) GOTO 903
      IF(IL.LT.0) GOTO 904
      IF(IM.NE.0) GOTO 905

C  MAKE SURE BOTH FILES HAVE THE SAME TABLES
C  -----------------------------------------

      SUBSET = TAG(INODE(LIN))(1:8)
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
