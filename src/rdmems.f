C> @file
C> @brief Read a specified data subset from a BUFR message.

C> This subroutine reads a specified data subset from the BUFR message
C> that was most recently read via a call to subroutine rdmemm() or
C> readmm().
C>
C> @author J. Woollen
C> @date 1994-01-06
C>
C> @param[in] ISUB   -- integer: Number of data subset to be
C>                      read from BUFR message, counting from the
C>                      beginning of the message
C> @param[out] IRET  -- integer: return code
C>                         - 0 = requested data subset was
C>                               successfully read
C>                         - -1 = requested subset number could not
C>                                be found in the message
C>
C> <p>Whenever this subroutine returns with IRET = 0, this indicates
C> that a new BUFR data subset (i.e. report) was successfully read into
C> internal arrays within the BUFRLIB software, and from where it can
C> now be easily manipulated or further parsed via calls to any of the
C> [values-reading subroutines](@ref hierarchy) using the Fortran
C> logical unit number IUNIT that was returned from the most recent
C> call to subroutine ufbmem().
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 1994-01-06 | J. Woollen | Original author |
C> | 1998-07-08 | J. Woollen | Replaced call to Cray library routine "ABORT" with call to new internal routine bort() |
C> | 1998-10-27 | J. Woollen | Modified to correct problems caused by in-lining code with fpp directives |
C> | 1999-11-18 | J. Woollen | The number of BUFR files which can be opened at one time increased from 10 to 32 |
C> | 2000-09-19 | J. Woollen | Maximum message length increased from 10,000 to 20,000 bytes |
C> | 2001-08-15 | D. Keyser  | Increased MAXMEM from 8 Mb to 16 Mb |
C> | 2004-08-09 | J. Ator    | Maximum message length increased from 20,000 to 50,000 bytes |
C> | 2004-11-15 | D. Keyser  | Increased MAXMEM from 16 Mb to 50 Mb |
C> | 2009-04-21 | J. Ator    | Use errwrt() |
C> | 2014-12-10 | J. Ator    | Use modules instead of COMMON blocks |
C>
      SUBROUTINE RDMEMS(ISUB,IRET)

      USE MODA_MSGCWD
      USE MODA_UNPTYP
      USE MODA_BITBUF
      USE MODA_MSGMEM

      CHARACTER*128 BORT_STR,ERRSTR

      COMMON /QUIET / IPRT

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK THE MESSAGE REQUEST AND FILE STATUS
C  -----------------------------------------

      CALL STATUS(MUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IL.GT.0) GOTO 901
      IF(IM.EQ.0) GOTO 902
      IF(NSUB(LUN).NE.0) GOTO 903

      IF(ISUB.GT.MSUB(LUN)) THEN
         IF(IPRT.GE.0) THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
           WRITE ( UNIT=ERRSTR, FMT='(A,I5,A,A,I5,A)' )
     .      'BUFRLIB: RDMEMS - REQ. SUBSET #', ISUB, ' (= 1st INPUT ',
     .      'ARG.) > # OF SUBSETS IN MEMORY MESSAGE (', MSUB(LUN), ')'
           CALL ERRWRT(ERRSTR)
           CALL ERRWRT('RETURN WITH IRET = -1')
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
         ENDIF
         IRET = -1
         GOTO 100
      ENDIF

      MBYM = MBYT(LUN)
      NBYT = 0

C  POSITION TO SUBSET NUMBER ISUB IN MEMORY MESSAGE
C  ------------------------------------------------

      IF(MSGUNP(LUN).EQ.0) THEN
         NSUB(LUN) = ISUB-1
         DO I=1,ISUB-1
         MBYT(LUN) = MBYT(LUN) + IUPB(MBAY(1,LUN),MBYT(LUN)+1,16)
         ENDDO
      ELSEIF(MSGUNP(LUN).EQ.1) THEN
c  .... message with "standard" Section 3
         DO I=1,ISUB-1
         CALL READSB(MUNIT,IRET)
         ENDDO
      ELSEIF(MSGUNP(LUN).EQ.2) THEN
c  .... compressed message
         NSUB(LUN) = ISUB-1
      ENDIF

C  NOW READ SUBSET NUMBER ISUB FROM MEMORY MESSAGE
C  -----------------------------------------------

      CALL READSB(MUNIT,IRET)
c  .... This should have already been accounted for with stmt. 902 or
c       IRET = -1 above
      IF(IRET.NE.0) GOTO 904

C  RESET SUBSET POINTER BACK TO ZERO (BEGINNING OF MESSAGE) AND RETURN
C  -------------------------------------------------------------------

      MBYT(LUN) = MBYM
      NSUB(LUN) = 0

C  EXITS
C  -----

100   RETURN
900   CALL BORT('BUFRLIB: RDMEMS - INPUT BUFR FILE IS CLOSED, IT '//
     . 'MUST BE OPEN FOR INPUT')
901   CALL BORT('BUFRLIB: RDMEMS - INPUT BUFR FILE IS OPEN FOR '//
     . 'OUTPUT, IT MUST BE OPEN FOR INPUT')
902   CALL BORT('BUFRLIB: RDMEMS - A MEMORY MESSAGE MUST BE OPEN IN '//
     . 'INPUT BUFR FILE, NONE ARE')
903   WRITE(BORT_STR,'("BUFRLIB: RDMEMS - UPON ENTRY, SUBSET POINTER '//
     . 'IN MEMORY MESSAGE IS NOT AT BEGINNING (",I3," SUBSETS HAVE '//
     . 'BEEN READ, SHOULD BE 0)")') NSUB(LUN)
      CALL BORT(BORT_STR)
904   CALL BORT('BUFRLIB: RDMEMS - CALL TO ROUTINE READSB RETURNED '//
     . 'WITH IRET = -1 (EITHER MEMORY MESSAGE NOT OPEN OR ALL '//
     . 'SUBSETS IN MESSAGE READ')
      END
