C> @file
C> @brief Read and parses a file containing a user-
c> supplied bufr dictionary table in character format, and then stores
c> this information into internal arrays in module tababd.
C> 
C> ### Program History Log
C> Date | Programmer | Comments 
C> -----|------------|----------
C> 1994-01-06 | J. Woollen | original author
C> 1995-06-28 | J. Woollen | increased the size of internal bufr table arrays in order to handle bigger files
C> 1996-12-17 | J. Woollen | fixed for some mvs compiler's treatment of internal reads (increases portability)
C> 1998-07-08 | J. Woollen | replaced call to cray library routine "abort" with call to new internal bufrlib routine "bort"; corrected some minor errors
C> 1999-11-18 | J. Woollen | the number of bufr files which can be opened at one time increased from 10 to 32
C> 2003-11-04 | J. Ator    | added documentation
C> 2003-11-04 | S. Bender  | added remarks/bufrlib routine interdependencies
C> 2003-11-04 | D. Keyser  | unified/portable for wrf; documentation; outputs diagnostic info; changed call from bort to bort2
C> 2006-04-14 | D. Keyser  | aborts if a user-defined message type "011" is read.
C> 2007-01-19 | J. Ator    | modified in response to numbck changes
C> 2009-03-23 | J. Ator    | increase size of bort_str2; use stntbia
C> 2013-01-08 | J. Whiting | add err= option to read statement
C> 2014-12-10 | J. Ator    | use modules instead of common blocks
C>
C> @author Woollen @date 1994-01-06
      
C> This subroutine reads and parses a file containing a user-
c> supplied bufr dictionary table in character format, and then stores
c> this information into internal arrays in module tababd (see remarks
c> for contents of internal arrays). This subroutine performs
c> a function similar to bufr archive library subroutine rdbfdx,
c> execpt that rdbfdx reads the bufr table directly from messages at
c> beginning of an input bufr file.
C>
C> @param[in] LUNDX - integer: fortran logical unit number for user-
C> supplied bufr dictionary table in character format.
C> @param[in] LUN - integer: i/o stream index into internal memory arrays.
C>
C> @author Woollen @date 1994-01-06
      SUBROUTINE RDUSDX(LUNDX,LUN)

      USE MODA_TABABD

      CHARACTER*128 BORT_STR1
      CHARACTER*156 BORT_STR2
      CHARACTER*80  CARD
      CHARACTER*8   NEMO
      CHARACTER*6   NUMB,NMB2

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  INITIALIZE THE DICTIONARY TABLE CONTROL WORD PARTITION ARRAYS
C  WITH APRIORI TABLE B AND D ENTRIES
C  --------------------------------------------------------------

      CALL DXINIT(LUN,1)
      REWIND LUNDX

C  READ USER CARDS UNTIL THERE ARE NO MORE
C  ---------------------------------------

1     READ(LUNDX,'(A80)',END=200,ERR=200) CARD

C  REREAD IF NOT A DEFINITION CARD
C  -------------------------------

c  .... This is a comment line
      IF(CARD(1: 1).EQ.       '*') GOTO 1
c  .... This is a separation line
      IF(CARD(3:10).EQ.'--------') GOTO 1
c  .... This is a blank line
      IF(CARD(3:10).EQ.'        ') GOTO 1
c  .... This is a header line
      IF(CARD(3:10).EQ.'MNEMONIC') GOTO 1
c  .... This is a header line
      IF(CARD(3:10).EQ.'TABLE  D') GOTO 1
c  .... This is a header line
      IF(CARD(3:10).EQ.'TABLE  B') GOTO 1

C  PARSE A DESCRIPTOR DEFINITION CARD
C  ----------------------------------

      IF(CARD(12:12).EQ.'|' .AND. CARD(21:21).EQ.'|') THEN

c  .... NEMO is the 8-character mnemonic name
         NEMO = CARD(3:10)
         IRET=NEMOCK(NEMO)
         IF(IRET.EQ.-1) GOTO 900
         IF(IRET.EQ.-2) GOTO 901

c  .... NUMB is the 6-character FXY value corresponding to NEMO
         NUMB = CARD(14:19)
         NMB2 = NUMB
         IF(NMB2(1:1).EQ.'A') NMB2(1:1) = '3'
         IRET=NUMBCK(NMB2)
         IF(IRET.EQ.-1) GOTO 902
         IF(IRET.EQ.-2) GOTO 903
         IF(IRET.EQ.-3) GOTO 904
         IF(IRET.EQ.-4) GOTO 905

C  TABLE A DESCRIPTOR FOUND
C  ------------------------

         IF(NUMB(1:1).EQ.'A') THEN
	   N = IGETNTBI ( LUN, 'A' )
	   CALL STNTBIA ( N, LUN, NUMB, NEMO, CARD(23:) )
	   IF ( IDNA(N,LUN,1) .EQ. 11 ) GOTO 906
c  .... Replace "A" with "3" so Table D descriptor will be found in
c  .... card as well (see below)
	   NUMB(1:1) = '3'
         ENDIF

C  TABLE B DESCRIPTOR FOUND
C  ------------------------

         IF(NUMB(1:1).EQ.'0') THEN
	   CALL STNTBI ( IGETNTBI(LUN,'B'), LUN, NUMB, NEMO, CARD(23:) )
           GOTO 1
         ENDIF

C  TABLE D DESCRIPTOR FOUND
C  ------------------------

         IF(NUMB(1:1).EQ.'3') THEN
	   CALL STNTBI ( IGETNTBI(LUN,'D'), LUN, NUMB, NEMO, CARD(23:) )
           GOTO 1
         ENDIF

c  .... First character of NUMB is not 'A', '0' or '3'
         GOTO 902

      ENDIF

C  PARSE A SEQUENCE DEFINITION CARD
C  --------------------------------

      IF(CARD(12:12).EQ.'|' .AND. CARD(19:19).NE.'|') THEN
         CALL SEQSDX(CARD,LUN)
         GOTO 1
      ENDIF

C  PARSE AN ELEMENT DEFINITION CARD
C  --------------------------------

      IF(CARD(12:12).EQ.'|' .AND. CARD(19:19).EQ.'|') THEN
         CALL ELEMDX(CARD,LUN)
         GOTO 1
      ENDIF

C  CAN'T FIGURE OUT WHAT KIND OF CARD IT IS
C  ----------------------------------------

      GOTO 907

C  NORMAL ENDING
C  -------------

200   CALL MAKESTAB

C  EXITS
C  -----

      RETURN
900   WRITE(BORT_STR1,'("BUFRLIB: RDUSDX - CARD READ IN IS: ",A)') CARD
      WRITE(BORT_STR2,'(18X,"MNEMONIC ",A," IN USER DICTIONARY IS NOT'//
     . ' BETWEEN 1 AND 8 CHARACTERS")') NEMO
      CALL BORT2(BORT_STR1,BORT_STR2)
901   WRITE(BORT_STR1,'("BUFRLIB: RDUSDX - CARD READ IN IS: ",A)') CARD
      WRITE(BORT_STR2,'(18X,"MNEMONIC ",A," IN USER DICTIONARY HAS '//
     . 'INVALID CHARACTERS")') NEMO
      CALL BORT2(BORT_STR1,BORT_STR2)
902   WRITE(BORT_STR1,'("BUFRLIB: RDUSDX - CARD READ IN IS: ",A)') CARD
      WRITE(BORT_STR2,'(18X,"DESCRIPTOR NUMBER ",A," IN USER '//
     . 'DICTIONARY HAS AN INVALID FIRST CHARACTER (F VALUE) - MUST BE'//
     . ' A, 0 OR 3")') NUMB
      CALL BORT2(BORT_STR1,BORT_STR2)
903   WRITE(BORT_STR1,'("BUFRLIB: RDUSDX - CARD READ IN IS: ",A)') CARD
      WRITE(BORT_STR2,'(18X,"DESCRIPTOR NUMBER ",A," IN USER '//
     . 'DICTIONARY HAS NON-NUMERIC VALUES IN CHARACTERS 2-6 (X AND Y '//
     . 'VALUES)")') NUMB
      CALL BORT2(BORT_STR1,BORT_STR2)
904   WRITE(BORT_STR1,'("BUFRLIB: RDUSDX - CARD READ IN IS: ",A)') CARD
      WRITE(BORT_STR2,'(18X,"DESCRIPTOR NUMBER ",A," IN USER '//
     . 'DICTIONARY HAS INVALID NUMBER IN CHARACTERS 2-3 (X VALUE) - '//
     . 'MUST BE BETWEEN 00 AND 63")') NUMB
      CALL BORT2(BORT_STR1,BORT_STR2)
905   WRITE(BORT_STR1,'("BUFRLIB: RDUSDX - CARD READ IN IS: ",A)') CARD
      WRITE(BORT_STR2,'(18X,"DESCRIPTOR NUMBER ",A," IN USER '//
     . 'DICTIONARY HAS INVALID NUMBER IN CHARACTERS 4-6 (Y VALUE) - '//
     . 'MUST BE BETWEEN 000 AND 255")') NUMB
      CALL BORT2(BORT_STR1,BORT_STR2)
906   WRITE(BORT_STR1,'("BUFRLIB: RDUSDX - CARD READ IN IS: ",A)') CARD
      WRITE(BORT_STR2,'(18X,"USER-DEFINED MESSAGE TYPE ""011"" IS '//
     . 'RESERVED FOR DICTIONARY MESSAGES")')
      CALL BORT2(BORT_STR1,BORT_STR2)
907   WRITE(BORT_STR1,'("BUFRLIB: RDUSDX - CARD READ IN IS: ",A)') CARD
      WRITE(BORT_STR2,'(18X,"THIS CARD HAS A BAD FORMAT - IT IS NOT '//
     . 'RECOGNIZED BY THIS SUBROUTINE")')
      CALL BORT2(BORT_STR1,BORT_STR2)

      END
