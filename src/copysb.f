C> @file
C> @brief Copy a BUFR data subset.
C>
C> @author J. Woollen @date 1994-01-06

C> This subroutine copies a BUFR data subset from one Fortran logical
C> unit to another.
C>
C> Logical unit LUNIN should have already been opened for input
C> operations via a previous call to subroutine openbf(), and a BUFR
C> message should have already been read into internal arrays for
C> LUNIN via a previous call to one of the
C> [message-reading subroutines](@ref hierarchy).
C>
C> Logical unit LUNOT should have already been opened for output
C> operations via a previous call to subroutine openbf(), and a BUFR
C> message should already be open for output within internal arrays
C> via a previous call to one of the BUFRLIB
C> [message-writing subroutines](@ref hierarchy).
C>
C> The compression status of the data subset (i.e. compressed or
C> uncompressed) will be preserved when copying from LUNIN to LUNOT.
C>
C> If LUNOT < 0, then a data subset is read from the BUFR message
C> in internal arrays for LUNIN but is not copied to the BUFR
C> message in internal arrays for LUNOT.  Otherwise, the
C> [DX BUFR Table information](@ref dfbftab) associated with
C> each of the logical units LUNIN and LUNOT must contain identical
C> definitions for the type of BUFR message containing the data
C> subset to be copied from LUNIN to LUNOT.
C>
C> @param[in] LUNIN   -- integer: Fortran logical unit number for
C>                       source BUFR file
C> @param[in] LUNOT   -- integer: Fortran logical unit number for
C>                       target BUFR file
C> @param[out] IRET   -- integer: return code
C>                       - 0 = normal return
C>                       - -1 = a BUFR data subset could not be
C>                              read from the BUFR message in
C>                              internal arrays for LUNIN
C>
C> @author J. Woollen @date 1994-01-06
      RECURSIVE SUBROUTINE COPYSB(LUNIN,LUNOT,IRET)

      USE MODA_MSGCWD
      USE MODA_BITBUF
      USE MODA_TABLES
      USE MODV_IM8B

      CHARACTER*128 BORT_STR

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK FOR I8 INTEGERS
C  ---------------------

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84(LUNIN,MY_LUNIN,1)
         CALL X84(LUNOT,MY_LUNOT,1)
         CALL COPYSB(MY_LUNIN,MY_LUNOT,IRET)
         CALL X48(IRET,IRET,1)

         IM8B=.TRUE.
         RETURN
      ENDIF

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
