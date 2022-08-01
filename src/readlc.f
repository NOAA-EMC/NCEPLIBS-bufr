C> @file
C> @brief Read a long character string (greater than 8 bytes) from
C> a data subset.

C> This subroutine reads a long character string (greater than 8 bytes)
C> from a data subset.
C>
C> <p>The data subset should have already been read into internal arrays
C> via a previous call to one of the
C> [subset-reading subroutines](@ref hierarchy).
C>
C> @authors J. Woollen
C> @authors J. Ator
C> @date 2003-11-04
C>
C> @param[in] LUNIT -- integer: Fortran logical unit number for BUFR file
C> @param[out] CHR  -- character*(*): Value corresponding to STR
C> @param[in] STR   -- character*(*): Table B mnemonic of long character
C>                     string to be retrieved, possibly supplemented
C>                     with an ordinal occurrence notation
C>
C> <p>If there is more than one occurrence of STR within the data subset
C> definition, then each occurrence can be retrieved via a separate call
C> to this subroutine, and by appending the ordinal number of the
C> occurrence to STR in each case.  For example, if there are 5
C> occurrences of mnemonic LSTID within a given data subset definition,
C> then 5 separate calls should be made to this subroutine, once each
C> with STR set to 'LSTID#1', 'LSTID#2', 'LSTID#3', 'LSTID#4' and
C> 'LSTID#5'.  However, the first notation is superfluous, because
C> omitting the ordinal number always defaults to the first occurrence
C> of a particular string, so a user could just specify 'LSTID'
C> instead of 'LSTID#1'.
C>
C> @remarks
C> - Character strings which are 8 bytes or less in length can be read
C> using the real*8 USR array within a call to one of the BUFRLIB
C> [values-reading subroutines](@ref hierarchy) and then converting the
C> corresponding real*8 value to character format within the
C> application program.
C> - If STR is not found within the data subset definition, then CHR is
C> returned with all bits set to 1, which is the standard WMO BUFR value
C> for "missing" data.  Any CHR value returned by this subroutine can be
C> checked for equivalence to this "missing" value via a call to
C> function icbfms().
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 2003-11-04 | J. Woollen | Original author |
C> | 2004-08-09 | J. Ator | Maximum message length increased from 20,000 to 50,000 bytes |
C> | 2007-01-19 | J. Ator | Replaced call to parseq with call to parstr() |
C> | 2009-03-23 | J. Ator | Added capability for compressed messages; added check for overflow of chr; added '#' option for more than one occurrence of STR |
C> | 2009-04-21 | J. Ator | Use errwrt() |
C> | 2012-12-07 | J. Ator | Allow str mnemonic length of up to 14 chars when used with '#' occurrence code |
C> | 2014-12-10 | J. Ator | Use modules instead of COMMON blocks |
C> | 2020-09-09 | J. Ator | Set CHR to "missing" instead of all blanks if STR isn't found in subset |
C>
C--------------------------------------------------------------------------
C--------------------------------------------------------------------------
      SUBROUTINE READLC_8(LUNIT,CHR,STR)
      INTEGER*8 LUNIT
      LUNIT4=LUNIT
      CALL READLC(LUNIT4,CHR,STR)
      END SUBROUTINE
C--------------------------------------------------------------------------
C--------------------------------------------------------------------------

      SUBROUTINE READLC(LUNIT,CHR,STR)

      USE MODA_USRINT
      USE MODA_USRBIT
      USE MODA_UNPTYP
      USE MODA_BITBUF
      USE MODA_TABLES
      USE MODA_RLCCMN
      USE MODA_IM8B

      COMMON /QUIET / IPRT

      CHARACTER*(*) CHR,STR
      CHARACTER*128 BORT_STR,ERRSTR
      CHARACTER*10  CTAG
      CHARACTER*14  TGS(10)

      DATA MAXTG /10/

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK FOR I8 INTEGERS
C  ---------------------
      IF(IM8) THEN
         IM8=.FALSE.
         CALL READLC_8(LUNIT,CHR,STR)
         IM8=.TRUE.
         RETURN
      ENDIF

      CHR = ' '
      LCHR=LEN(CHR)

C  CHECK THE FILE STATUS
C  ---------------------

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IL.GT.0) GOTO 901
      IF(IM.EQ.0) GOTO 902

C  CHECK FOR TAGS (MNEMONICS) IN INPUT STRING (THERE CAN ONLY BE ONE)
C  ------------------------------------------------------------------

      CALL PARSTR(STR,TGS,MAXTG,NTG,' ',.TRUE.)
      IF(NTG.GT.1) GOTO 903

C     Check if a specific occurrence of the input string was requested;
C     if not, then the default is to return the first occurrence.

      CALL PARUTG(LUN,0,TGS(1),NNOD,KON,ROID)
      IF(KON.EQ.6) THEN
         IOID=NINT(ROID)
         IF(IOID.LE.0) IOID = 1
         CTAG = ' '
         II = 1
         DO WHILE((II.LE.10).AND.(TGS(1)(II:II).NE.'#'))
            CTAG(II:II)=TGS(1)(II:II)
            II = II + 1
         ENDDO
      ELSE
         IOID = 1
         CTAG = TGS(1)(1:10)
      ENDIF

C  LOCATE AND DECODE THE LONG CHARACTER STRING
C  -------------------------------------------

      IF(MSGUNP(LUN).EQ.0.OR.MSGUNP(LUN).EQ.1) THEN

C        The message is uncompressed

         ITAGCT = 0
         DO N=1,NVAL(LUN)
           NOD = INV(N,LUN)
           IF(CTAG.EQ.TAG(NOD)) THEN
             ITAGCT = ITAGCT + 1
             IF(ITAGCT.EQ.IOID) THEN
               IF(ITP(NOD).NE.3) GOTO 904
               NCHR = NBIT(N)/8
               IF(NCHR.GT.LCHR) GOTO 905
               KBIT = MBIT(N)
               CALL UPC(CHR,NCHR,MBAY(1,LUN),KBIT,.TRUE.)
               GOTO 100
             ENDIF
           ENDIF
         ENDDO
      ELSEIF(MSGUNP(LUN).EQ.2) THEN

C        The message is compressed

         IF(NRST.GT.0) THEN
           ITAGCT = 0
           DO II=1,NRST
             IF(CTAG.EQ.CRTAG(II)) THEN
               ITAGCT = ITAGCT + 1
               IF(ITAGCT.EQ.IOID) THEN
                 NCHR = IRNCH(II)
                 IF(NCHR.GT.LCHR) GOTO 905
                 KBIT = IRBIT(II)
                 CALL UPC(CHR,NCHR,MBAY(1,LUN),KBIT,.TRUE.)
                 GOTO 100
               ENDIF
             ENDIF
           ENDDO
         ENDIF
      ELSE
         GOTO 906
      ENDIF

C     If we made it here, then we couldn't find the requested string.

      IF(IPRT.GE.0) THEN
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
      ERRSTR = 'BUFRLIB: READLC - MNEMONIC ' // TGS(1) //
     .   ' NOT LOCATED IN REPORT SUBSET - RETURN WITH MISSING' //
     .   ' STRING FOR CHARACTER DATA ELEMENT'
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
      CALL ERRWRT(' ')
      ENDIF
      DO II=1,LCHR
        CALL IPKM(CHR(II:II),1,255)
      ENDDO

C  EXITS
C  -----

100   RETURN
900   CALL BORT('BUFRLIB: READLC - INPUT BUFR FILE IS CLOSED, IT MUST'//
     . ' BE OPEN FOR INPUT')
901   CALL BORT('BUFRLIB: READLC - INPUT BUFR FILE IS OPEN FOR '//
     . 'OUTPUT, IT MUST BE OPEN FOR INPUT')
902   CALL BORT('BUFRLIB: READLC - A MESSAGE MUST BE OPEN IN INPUT '//
     . 'BUFR FILE, NONE ARE')
903   WRITE(BORT_STR,'("BUFRLIB: READLC - THERE CANNOT BE MORE THAN '//
     . 'ONE MNEMONIC IN THE INPUT STRING (",A,") (HERE THERE ARE ",'//
     . 'I3,")")') STR,NTG
      CALL BORT(BORT_STR)
904   WRITE(BORT_STR,'("BUFRLIB: READLC - MNEMONIC ",A," DOES NOT '//
     . 'REPRESENT A CHARACTER ELEMENT (ITP=",I2,")")') TGS(1),ITP(NOD)
      CALL BORT(BORT_STR)
905   WRITE(BORT_STR,'("BUFRLIB: READLC - MNEMONIC ",A," IS A '//
     . 'CHARACTER STRING OF LENGTH",I4," BUT SPACE WAS PROVIDED '//
     . 'FOR ONLY",I4, " CHARACTERS")') TGS(1),NCHR,LCHR
      CALL BORT(BORT_STR)
906   WRITE(BORT_STR,'("BUFRLIB: READLC - MESSAGE UNPACK TYPE",I3,'//
     . '" IS NOT RECOGNIZED")') MSGUNP
      CALL BORT(BORT_STR)
      END
