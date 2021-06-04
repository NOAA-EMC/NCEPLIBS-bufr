C> @file
C> @brief Copy a BUFR data subset.

C> This subroutine copies a BUFR data subset from one Fortran logical
C> unit to another.
C>
C> @author J. Woollen
C> @date 1994-01-06
C>
C> @param[in] LUNIN    - integer: Fortran logical unit number for
C>                       source BUFR file
C> @param[in] LUNOT    - integer: Fortran logical unit number for
C>                       target BUFR file
C> @param[out] IRET    - integer: return code
C>                       - 0 = normal return
C>                       - -1 = a BUFR data subset could not be
C>                              read from the BUFR message in
C>                              internal arrays for LUNIN
C>
C> <p>Logical unit LUNIN should have already been opened for input
C> operations via a previous call to subroutine openbf(), and a BUFR
C> message should have already been read into internal arrays for
C> LUNIN via a previous call to one of the
C> [message-reading subroutines](@ref hierarchy).
C>
C> <p>Logical unit LUNOT should have already been opened for output
C> operations via a previous call to subroutine openbf(), and a BUFR
C> message should already be open for output within internal arrays
C> via a previous call to one of the BUFRLIB
C> [message-writing subroutines](@ref hierarchy).
C>
C> <p>The compression status of the data subset (i.e. compressed or
C> uncompressed) will be preserved when copying from LUNIN to LUNOT.
C>
C> <p>If LUNOT < 0, then a data subset is read from the BUFR message
C> in internal arrays for LUNIN but is not copied to the BUFR
C> message in internal arrays for LUNOT.  Otherwise, the
C> [DX BUFR Table information](@ref dfbftab) associated with
C> each of the logical units LUNIN and LUNOT must contain identical
C> definitions for the type of BUFR message containing the data
C> subset to be copied from LUNIN to LUNOT.
C>
C> <b>Program history log:</b>
C> - 1994-01-06  J. Woollen -- Original author
C> - 1998-07-08  J. Woollen -- Replaced call to Cray library routine ABORT
C>                             with call to new internal routine bort()
C> - 1999-11-18  J. Woollen -- The number of BUFR files which can be
C>                             opened at one time increased from 10 to 32
C>                             (necessary in order to process multiple
C>                             BUFR files under the MPI)
C> - 2000-09-19  J. Woollen -- Maximum message length increased
C>                             from 10,000 to 20,000 bytes
C> - 2002-05-14  J. Woollen -- Removed old Cray compiler directives
C> - 2004-08-09  J. Ator    -- Maximum message length increased
C>                             from 20,000 to 50,000 bytes
C> - 2005-09-16  J. Woollen -- Now writes out compressed subset/message if
C>                           input subset/message is compressed (before
C>                           could only write out uncompressed subset/
C>                           message regardless of compression status of
C>                           input subset/message)
C> - 2009-06-26  J. Ator    -- Use iok2cpy()
C> - 2014-11-03  J. Ator    -- Handle oversized (>65530 bytes) subsets
C> - 2014-12-10  J. Ator    -- Use modules instead of COMMON blocks
C>
      SUBROUTINE COPYSB(LUNIN,LUNOT,IRET)

      USE MODA_MSGCWD
      USE MODA_BITBUF
      USE MODA_TABLES

      CHARACTER*128 BORT_STR

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      IRET = 0

C  CHECK THE FILE STATUSES
C  -----------------------

      CALL STATUS(LUNIN,LIN,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IL.GT.0) GOTO 901
      IF(IM.EQ.0) GOTO 902

      IF(LUNOT.GT.0) THEN
         CALL STATUS(LUNOT,LOT,IL,IM)
         IF(IL.EQ.0) GOTO 903
         IF(IL.LT.0) GOTO 904
         IF(IM.EQ.0) GOTO 905
         IF(INODE(LIN).NE.INODE(LOT)) THEN
           IF( (TAG(INODE(LIN)).NE.TAG(INODE(LOT))) .OR.
     .        (IOK2CPY(LIN,LOT).NE.1) ) GOTO 906
         ENDIF
      ENDIF

C  SEE IF THERE IS ANOTHER SUBSET IN THE MESSAGE
C  ---------------------------------------------

      IF(NSUB(LIN).EQ.MSUB(LIN)) THEN
         IRET = -1
         GOTO 100
      ENDIF

C  CHECK COMPRESSION STATUS OF INPUT MESSAGE, OUTPUT MESSAGE WILL MATCH
C  --------------------------------------------------------------------

      CALL MESGBC(-LUNIN,MEST,ICMP)

      IF(ICMP.EQ.1) THEN

C  -------------------------------------------------------
C  THIS BRANCH IS FOR COMPRESSED INPUT/OUTPUT MESSAGES
C  -------------------------------------------------------
C  READ IN AND UNCOMPRESS SUBSET, THEN COPY IT TO COMPRESSED OUTPUT MSG
C  --------------------------------------------------------------------

         CALL READSB(LUNIN,IRET)
         IF(LUNOT.GT.0) THEN
            CALL UFBCPY(LUNIN,LUNOT)
            CALL CMPMSG('Y')
            CALL WRITSB(LUNOT)
            CALL CMPMSG('N')
         ENDIF
         GOTO 100
      ELSE  IF(ICMP.EQ.0) THEN

C  -------------------------------------------------------
C  THIS BRANCH IS FOR UNCOMPRESSED INPUT/OUTPUT MESSAGES
C  -------------------------------------------------------
C  COPY THE SUBSET TO THE OUTPUT MESSAGE AND/OR RESET THE POINTERS
C  ---------------------------------------------------------------

         IBIT = (MBYT(LIN))*8
         CALL UPB(NBYT,16,MBAY(1,LIN),IBIT)
         IF (NBYT.GT.65530) THEN

C          This is an oversized subset, so we can't rely on the value
C          of NBYT as being the true size (in bytes) of the subset.

           IF ( (NSUB(LIN).EQ.0) .AND. (MSUB(LIN).EQ.1) )  THEN

C            But it's also the first and only subset in the message,
C            so we can determine its true size in a different way.

             CALL GETLENS(MBAY(1,LIN),4,LEN0,LEN1,LEN2,LEN3,LEN4,L5)
             NBYT = LEN4 - 4
           ELSE

C            We have no way to easily determine the true size of this
C            oversized subset.

             IRET = -1
             GOTO 100
           ENDIF
         ENDIF
         IF(LUNOT.GT.0) CALL CPYUPD(LUNOT,LIN,LOT,NBYT)
         MBYT(LIN) = MBYT(LIN) + NBYT
         NSUB(LIN) = NSUB(LIN) + 1
      ELSE
         GOTO 907
      ENDIF

C  EXITS
C  -----

100   RETURN
900   CALL BORT('BUFRLIB: COPYSB - INPUT BUFR FILE IS CLOSED, IT '//
     . 'MUST BE OPEN FOR INPUT')
901   CALL BORT('BUFRLIB: COPYSB - INPUT BUFR FILE IS OPEN FOR '//
     . 'OUTPUT, IT MUST BE OPEN FOR INPUT')
902   CALL BORT('BUFRLIB: COPYSB - A MESSAGE MUST BE OPEN IN INPUT '//
     . 'BUFR FILE, NONE ARE')
903   CALL BORT('BUFRLIB: COPYSB - OUTPUT BUFR FILE IS CLOSED, IT '//
     . 'MUST BE OPEN FOR OUTPUT')
904   CALL BORT('BUFRLIB: COPYSB - OUTPUT BUFR FILE IS OPEN FOR '//
     . 'INPUT, IT MUST BE OPEN FOR OUTPUT')
905   CALL BORT('BUFRLIB: COPYSB - A MESSAGE MUST BE OPEN IN OUTPUT '//
     . 'BUFR FILE, NONE ARE')
906   CALL BORT('BUFRLIB: COPYSB - INPUT AND OUTPUT BUFR FILES MUST '//
     . 'HAVE THE SAME INTERNAL TABLES, THEY ARE DIFFERENT HERE')
907   WRITE(BORT_STR,'("BUFRLIB: COPYSB - INVALID COMPRESSION '//
     . 'INDICATOR (ICMP=",I3," RETURNED FROM BUFR ARCHIVE LIBRARY '//
     . 'ROUTINE MESGBC")') ICMP
      CALL BORT(BORT_STR)
      END
