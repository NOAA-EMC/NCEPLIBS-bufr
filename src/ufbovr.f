C> @file
C> @brief Overwrite one or more data values within a data subset.
C>
C> ### Program History Log
C> Date | Programmer | Comments |
C> -----|------------|----------|
C> 1994-01-06 | J. Woollen | original author
C> 1998-07-08 | J. Woollen | replaced call to cray library routine "abort" with call to new internal bufrlib routine "bort"
C> 1999-11-18 | J. Woollen | the number of bufr files which can be opened at one time increased from 10 to 32
C> 2002-05-14 | J. Woollen | removed old cray compiler directives
C> 2003-11-04 | S. Bender  | added remarks/bufrlib routine interdependencies
C> 2003-11-04 | D. Keyser  | maxjl increased to 16000; unified/portable for wrf; documentation; outputs more 
C> 2004-08-18 | J. Ator    | added save for ifirst1 and ifirst2 flags
C> 2009-04-21 | J. Ator    | use errwrt
C> 2014-12-10 | J. Ator    | use modules instead of common blocks
C> 2015-09-24 | D. Stokes  | fix missing declaration of common /quiet/
C> 2022-10-04 | J. Ator    | added 8-byte wrapper
C>
C> @author Woollen @date 1994-01-06
      
C> This subroutine writes over specified values which exist
C> in current internal BUFR subset arrays in a file open for output.
C> The data values correspond to mnemonics which are part of a
C> delayed-replication sequence, or for which there is no replication
C> at all. Either BUFR archive library subroutine openmg() or openmb()
C> must have been previously called to open and initialize a BUFR
C> message within memory for this lunit. In addition, BUFR archive
C> library subroutine writsb() or invmrg() must have been called to
C> store data in the internal output subset arrays.
C>
C> @param[in] LUNIT - integer: Fortran logical unit number for BUFR file.
C> @param[in] USR - real*8(*,*): data values
C> @param[in] I1 - integer: First dimension of USR as allocated within
C> the calling program.
C> @param[in] I2 - integer: Number of replications of STR that are to
C> be written into the data subset.
C> @param[out] IRET - integer: Number of replications of STR that were
C> written into the data subset.
C> @param[in] STR - character*(*): string of blank-separated Table B
C> mnemonics in one-to-one correspondence with first dimension of USR.
C>
C> @author Woollen @date 1994-01-06
      RECURSIVE SUBROUTINE UFBOVR(LUNIT,USR,I1,I2,IRET,STR)

      USE MODV_IM8B

      USE MODA_USRINT
      USE MODA_MSGCWD

      COMMON /QUIET / IPRT

      CHARACTER*128 BORT_STR1,BORT_STR2,ERRSTR
      CHARACTER*(*) STR
      REAL*8        USR(I1,I2)

      DATA IFIRST1/0/,IFIRST2/0/

      SAVE IFIRST1, IFIRST2

C----------------------------------------------------------------------
C----------------------------------------------------------------------

C  CHECK FOR I8 INTEGERS
C  ---------------------

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84(LUNIT,MY_LUNIT,1)
         CALL X84(I1,MY_I1,1)
         CALL X84(I2,MY_I2,1)
         CALL UFBOVR(MY_LUNIT,USR,MY_I1,MY_I2,IRET,STR)
         CALL X48(IRET,IRET,1)

         IM8B=.TRUE.
         RETURN
      ENDIF

      IRET = 0

C  CHECK THE FILE STATUS AND I-NODE
C  --------------------------------

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IL.LT.0) GOTO 901
      IF(IM.EQ.0) GOTO 902
      IF(INODE(LUN).NE.INV(1,LUN)) GOTO 903

C  .... DK: Why check, isn't IO always 1 here?
      IO = MIN(MAX(0,IL),1)

      IF(I1.LE.0) THEN
         IF(IPRT.GE.0) THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      ERRSTR = 'BUFRLIB: UFBOVR - 3rd ARG. (INPUT) IS .LE. 0, ' //
     .   'SO RETURN WITH 5th ARG. (IRET) = 0; 6th ARG. (STR) ='
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT(STR)
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
         ENDIF
         GOTO 100
      ELSEIF(I2.LE.0) THEN
         IF(IPRT.EQ.-1)  IFIRST1 = 1
         IF(IO.EQ.0 .OR. IFIRST1.EQ.0 .OR. IPRT.GE.1)  THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      ERRSTR = 'BUFRLIB: UFBOVR - 4th ARG. (INPUT) IS .LE. 0, ' //
     .   'SO RETURN WITH 5th ARG. (IRET) = 0; 6th ARG. (STR) ='
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT(STR)
            IF(IPRT.EQ.0 .AND. IO.EQ.1) THEN
      ERRSTR = 'Note: Only the first occurrence of this WARNING ' //
     .   'message is printed, there may be more.  To output all ' //
     .   'such messages,'
      CALL ERRWRT(ERRSTR)
      ERRSTR = 'modify your application program to add ' //
     .   '"CALL OPENBF(0,''QUIET'',1)" prior to the first call ' //
     .   'to a BUFRLIB routine.'
      CALL ERRWRT(ERRSTR)
            ENDIF
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
            IFIRST1 = 1
         ENDIF
         GOTO 100
      ENDIF

C  PARSE OR RECALL THE INPUT STRING - READ/WRITE VALUES
C  ----------------------------------------------------

      CALL STRING(STR,LUN,I1,IO)
      CALL TRYBUMP(LUNIT,LUN,USR,I1,I2,IO,IRET)

      IF(IO.EQ.1 .AND. IRET.NE.I2) GOTO 904

      IF(IRET.EQ.0)  THEN
         IF(IPRT.EQ.-1)  IFIRST2 = 1
         IF(IFIRST2.EQ.0 .OR. IPRT.GE.1)  THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      ERRSTR = 'BUFRLIB: UFBOVR - NO SPECIFIED VALUES WRITTEN OUT, ' //
     .   'SO RETURN WITH 5th ARG. (IRET) = 0; 6th ARG. (STR) ='
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT(STR)
      CALL ERRWRT('MAY NOT BE IN THE BUFR TABLE(?)')
               IF(IPRT.EQ.0) THEN
      ERRSTR = 'Note: Only the first occurrence of this WARNING ' //
     .   'message is printed, there may be more.  To output all ' //
     .   'such messages,'
      CALL ERRWRT(ERRSTR)
      ERRSTR = 'modify your application program to add ' //
     .   '"CALL OPENBF(0,''QUIET'',1)" prior to the first call ' //
     .   'to a BUFRLIB routine.'
      CALL ERRWRT(ERRSTR)
               ENDIF
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
            IFIRST2 = 1
         ENDIF
      ENDIF

C  EXITS
C  -----

100   RETURN
900   CALL BORT('BUFRLIB: UFBOVR - OUTPUT BUFR FILE IS CLOSED, IT '//
     . 'MUST BE OPEN FOR OUTPUT')
901   CALL BORT('BUFRLIB: UFBOVR - OUTPUT BUFR FILE IS OPEN FOR '//
     . 'INPUT, IT MUST BE OPEN FOR OUTPUT')
902   CALL BORT('BUFRLIB: UFBOVR - A MESSAGE MUST BE OPEN IN OUTPUT '//
     . 'BUFR FILE, NONE ARE')
903   CALL BORT('BUFRLIB: UFBOVR - LOCATION OF INTERNAL TABLE FOR '//
     . 'OUTPUT BUFR FILE DOES NOT AGREE WITH EXPECTED LOCATION IN '//
     . 'INTERNAL SUBSET ARRAY')
904   WRITE(BORT_STR1,'("BUFRLIB: UFBOVR - MNEMONIC STRING READ IN IS'//
     . ': ",A)') STR
      WRITE(BORT_STR2,'(18X,"THE NUMBER OF ''LEVELS'' ACTUALLY '//
     . 'WRITTEN (",I3,") DOES NOT EQUAL THE NUMBER REQUESTED (",I3,")'//
     . ' - INCOMPLETE WRITE")')  IRET,I2
      CALL BORT2(BORT_STR1,BORT_STR2)
      END
