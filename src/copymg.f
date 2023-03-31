C> @file
C> @brief Copy a BUFR message from one file to another.
C>
C> @author J. Woollen @date 1994-01-06

C> Copy a BUFR message from one file to another.
C>
C> This subroutine is similar to subroutine cpymem(), except that
C> it copies a BUFR message from one Fortran logical unit to another,
C> whereas cpymem() copies a BUFR message from internal arrays in
C> memory to a specified Fortran logical unit.
C>
C> Logical unit LUNIN should have already been opened for input
C> operations via a previous call to subroutine openbf(). A BUFR
C> message should have already been read into internal arrays for
C> LUNIN via a previous call to one of the
C> [message-reading subroutines](@ref hierarchy).
C>
C> Logical unit LUNOT should have already been opened for output
C> operations via a previous call to subroutine openbf(), but there
C> should not be any BUFR message already open for output within the
C> internal arrays for LUNOT via a previous call to one of the
C> [message-writing subroutines](@ref hierarchy).
C>
C> The [DX BUFR Table information](@ref dfbftab) associated with
C> each of the logical units LUNIN and LUNOT must contain identical
C> definitions for the type of BUFR message to be copied from LUNIN
C> to LUNOT.
C>
C> This subroutine uses subroutine msgwrt() to write to LUNOT;
C> therefore, it can be used to transform a copy of the
C> original BUFR message from LUNIN with any or all of the updates
C> described in the documentation for subroutine msgwrt().
C>
C> @param[in] LUNIN - integer: Fortran logical unit number for
C> source BUFR file.
C> @param[in] LUNOT - integer: Fortran logical unit number for
C> target BUFR file.
C>
C> @author J. Woollen @date 1994-01-06
      RECURSIVE SUBROUTINE COPYMG(LUNIN,LUNOT)

      USE MODA_MSGCWD
      USE MODA_BITBUF
      USE MODA_TABLES
      USE MODV_IM8B

      CHARACTER*8  SUBSET

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK FOR I8 INTEGERS
C  ---------------------

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84(LUNIN,MY_LUNIN,1)
         CALL X84(LUNOT,MY_LUNOT,1)
         CALL COPYMG(MY_LUNIN,MY_LUNOT)

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
