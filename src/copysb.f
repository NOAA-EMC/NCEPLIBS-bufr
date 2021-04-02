C> @file
C> @author WOOLLEN @date 1994-01-06
      
C> This subroutine copies a packed data subset, intact, from
c>   logical unit lunin, opened for input via a previous call to bufr
c>   archive library subroutine openbf, to logical unit lunot, opened
c>   for output via a previous call to openbf. The bufr message must
c>   have been previously read from unit lunit using bufr archive
c>   library subroutine readmg or readerme and may be either compressed
c>   or uncompressed. Also, bufr archive library subroutine openmg or
c>   openmb must have been previously called to open and initialize a
c>   bufr message within memory for unit lunot. Each call to copysb
c>   advances the pointer to the beginning of the next subset in both
c>   the input and output files, unless input parameter lunot is .le.
c>   zero, in which case the output pointer is not advanced. The
c>   compression status of the output subset/bufr message will always
c>   match that of the input subset/bufr message {i.e., if input message
c>   is uncompressed(compressed) output message will be uncompressed
c>   (compressed)}.
C>
C> PROGRAM HISTORY LOG:
C> -1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
C> -1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE
C>                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB
C>                           ROUTINE "BORT"
C> -1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE
C>                           OPENED AT ONE TIME INCREASED FROM 10 TO 32
C>                           (NECESSARY IN ORDER TO PROCESS MULTIPLE
C>                           BUFR FILES UNDER THE MPI)
C> -2000-09-19  J. WOOLLEN -- MAXIMUM MESSAGE LENGTH INCREASED FROM
C>                           10,000 TO 20,000 BYTES
C> -2002-05-14  J. WOOLLEN -- REMOVED OLD CRAY COMPILER DIRECTIVES
C> -2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C>                           INTERDEPENDENCIES
C> -2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED
C>                           DOCUMENTATION (INCLUDING HISTORY); OUTPUTS
C>                           MORE COMPLETE DIAGNOSTIC INFO WHEN ROUTINE
C>                           TERMINATES ABNORMALLY
C> -2004-08-09  J. ATOR    -- MAXIMUM MESSAGE LENGTH INCREASED FROM
C>                           20,000 TO 50,000 BYTES
C> -2005-09-16  J. WOOLLEN -- NOW WRITES OUT COMPRESSED SUBSET/MESSAGE IF
C>                           INPUT SUBSET/MESSAGE IS COMPRESSED (BEFORE
C>                           COULD ONLY WRITE OUT UNCOMPRESSED SUBSET/
C>                           MESSAGE REGARDLESS OF COMPRESSION STATUS OF
C>                           INPUT SUBSET/MESSAGE)
C> -2009-06-26  J. ATOR    -- USE IOK2CPY
C> -2014-11-03  J. ATOR    -- HANDLE OVERSIZED (>65530 BYTE) SUBSETS
C> -2014-12-10  J. ATOR    -- USE MODULES INSTEAD OF COMMON BLOCKS
C>
C> @param[in] LUNIN Fortran logical unit number for input bufr file.
C> @param[in] LUNOT Fortran logical unit number for output bufr file.
C> @param[out] IRET RETURN CODE:
C> -     0 = normal return
C> -    -1 = there are no more subsets in the input BUFR message
C>
C> REMARKS:
C> THIS ROUTINE CALLS: bort() cmpmsg() cpyupd() getlens()
C> iok2cpy() mesgbc() readsb() status() ufbcpy() upb() writsb()
C>    THIS ROUTINE IS CALLED BY: icopysb()
C>                               Also called by application programs.
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
