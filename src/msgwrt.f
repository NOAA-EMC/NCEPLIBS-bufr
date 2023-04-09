C> @file
C> @brief Finalize a BUFR message for output and write the message to
C> a BUFR file.
C>
C> @author J. Woollen @date 1994-01-06

C> Perform final checks and updates on a BUFR message
C> before writing it to a specified Fortran logical unit.
C>
C> These final checks and updates include:
C> - Standardizing the BUFR message, if requested via a previous call
C> subroutine stdmsg()
C> - Converting the BUFR message from edition 3 to edition 4, if
C> requested via a previous call to subroutine pkvs01()
C> - Storing any customized values into Section 0 or Section 1 of the
C> BUFR message, if requested via one or more previous calls to
C> subroutine pkvs01()
C> - Storing a tank receipt time into Section 1 of the BUFR message,
C> if requested via a previous call to subroutine strcpt()
C> - For edition 3 BUFR messages, ensuring each section of the message
C> contains an even number of bytes
C> - Storing '7777' into the last four bytes of the BUFR message, and
C> storing the final message length in Section 0
C> - Appending zeroed-out bytes after the end of the BUFR message, up
C> to the next machine word boundary
C> - Encapsulating the BUFR message with IEEE Fortran control words,
C> if requested via a previous call to subroutine setblock()
C> - Storing a copy of the final message into internal arrays for
C> possible later retrival via subroutine writsa()
C>
C> @param[in] LUNIT -- integer: Fortran logical unit number for BUFR
C>                     file
C> @param[in] MESG  -- integer(*): BUFR message
C> @param[in] MGBYT -- integer: Size (in bytes) of BUFR message
C>
C> @author J. Woollen @date 1994-01-06
      SUBROUTINE MSGWRT(LUNIT,MESG,MGBYT)

      use bufrlib

      USE MODV_MXMSGL

      USE MODA_NULBFR
      USE MODA_BUFRMG
      USE MODA_MGWA
      USE MODA_MGWB
      USE MODA_S01CM

      COMMON /QUIET / IPRT
      COMMON /MSGSTD/ CSMF
      COMMON /TNKRCP/ ITRYR,ITRMO,ITRDY,ITRHR,ITRMI,CTRT

      CHARACTER*128 ERRSTR

      CHARACTER*4 BUFR,SEVN
      CHARACTER*1 CSMF
      CHARACTER*1 CTRT
      DIMENSION   MESG(*)
      DIMENSION   IEC0(2)

      DATA BUFR/'BUFR'/
      DATA SEVN/'7777'/

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C     MAKE A LOCAL COPY OF THE INPUT MESSAGE FOR USE WITHIN THIS
C     SUBROUTINE, SINCE CALLS TO ANY OR ALL OF THE SUBROUTINES STNDRD,
C     CNVED4, PKBS1, ATRCPT, ETC. MAY END UP MODIFYING THE MESSAGE
C     BEFORE IT FINALLY GETS WRITTEN OUT TO LUNIT.

      MBYT = MGBYT

      IEC0(1) = MESG(1)
      IEC0(2) = MESG(2)
      IBIT = 32
      CALL PKB(MBYT,24,IEC0,IBIT)

      DO II = 1, NMWRD(IEC0)
        MGWA(II) = MESG(II)
      ENDDO

C     OVERWRITE ANY VALUES WITHIN SECTION 0 OR SECTION 1 THAT WERE
C     REQUESTED VIA PREVIOUS CALLS TO BUFR ARCHIVE LIBRARY SUBROUTINE
C     PKVS01.  IF A REQUEST WAS MADE TO CHANGE THE BUFR EDITION NUMBER
C     TO 4, THEN ACTUALLY CONVERT THE MESSAGE AS WELL.

      IF(NS01V.GT.0) THEN
        DO I=1,NS01V
          IF(CMNEM(I).EQ.'BEN') THEN
            IF(IVMNEM(I).EQ.4) THEN

C             INSTALL SECTION 0 BYTE COUNT FOR USE BY SUBROUTINE CNVED4.

              IBIT = 32
              CALL PKB(MBYT,24,MGWA,IBIT)

              CALL CNVED4(MGWA,MXMSGLD4,MGWB)

C             COMPUTE MBYT FOR THE NEW EDITION 4 MESSAGE.

              MBYT = IUPBS01(MGWB,'LENM')

C             COPY THE MGWB ARRAY BACK INTO MGWA.

              DO II = 1, NMWRD(MGWB)
                MGWA(II) = MGWB(II)
              ENDDO
            ENDIF
          ELSE

C           OVERWRITE THE REQUESTED VALUE.

            CALL PKBS1(IVMNEM(I),MGWA,CMNEM(I))
          ENDIF
        ENDDO
      ENDIF

C     "STANDARDIZE" THE MESSAGE IF REQUESTED VIA COMMON /MSGSTD/.
C     HOWEVER, WE DO NOT WANT TO DO THIS IF THE MESSAGE CONTAINS BUFR
C     TABLE (DX) INFORMATION, IN WHICH CASE IT IS ALREADY "STANDARD".

      IF ( ( CSMF.EQ.'Y' ) .AND. ( IDXMSG(MGWA).NE.1 ) )  THEN

C       INSTALL SECTION 0 BYTE COUNT AND SECTION 5 '7777' INTO THE
C       ORIGINAL MESSAGE.  THIS IS NECESSARY BECAUSE SUBROUTINE STNDRD
C       REQUIRES A COMPLETE AND WELL-FORMED BUFR MESSAGE AS ITS INPUT.

        IBIT = 32
        CALL PKB(MBYT,24,MGWA,IBIT)
        IBIT = (MBYT-4)*8
        CALL PKC(SEVN,4,MGWA,IBIT)

        CALL STNDRD(LUNIT,MGWA,MXMSGLD4,MGWB)

C       COMPUTE MBYT FOR THE NEW "STANDARDIZED" MESSAGE.

        MBYT = IUPBS01(MGWB,'LENM')

C       COPY THE MGWB ARRAY BACK INTO MGWA.

        DO II = 1, NMWRD(MGWB)
          MGWA(II) = MGWB(II)
        ENDDO
      ENDIF

C     APPEND THE TANK RECEIPT TIME TO SECTION 1 IF REQUESTED VIA
C     COMMON /TNKRCP/, UNLESS THE MESSAGE CONTAINS BUFR TABLE (DX)
C     INFORMATION.

      IF ( ( CTRT.EQ.'Y' ) .AND. ( IDXMSG(MGWA).NE.1 ) ) THEN

C       INSTALL SECTION 0 BYTE COUNT FOR USE BY SUBROUTINE ATRCPT.

        IBIT = 32
        CALL PKB(MBYT,24,MGWA,IBIT)

        CALL ATRCPT(MGWA,MXMSGLD4,MGWB)

C       COMPUTE MBYT FOR THE REVISED MESSAGE.

        MBYT = IUPBS01(MGWB,'LENM')

C       COPY THE MGWB ARRAY BACK INTO MGWA.

        DO II = 1, NMWRD(MGWB)
          MGWA(II) = MGWB(II)
        ENDDO
      ENDIF

C     GET THE SECTION LENGTHS.

      CALL GETLENS(MGWA,4,LEN0,LEN1,LEN2,LEN3,LEN4,L5)

C     DEPENDING ON THE EDITION NUMBER OF THE MESSAGE, WE NEED TO ENSURE
C     THAT EACH SECTION WITHIN THE MESSAGE HAS AN EVEN NUMBER OF BYTES.

      IF(IUPBS01(MGWA,'BEN').LT.4) THEN
        IF(MOD(LEN1,2).NE.0) GOTO 901
        IF(MOD(LEN2,2).NE.0) GOTO 902
        IF(MOD(LEN3,2).NE.0) GOTO 903
        IF(MOD(LEN4,2).NE.0) THEN

C          PAD SECTION 4 WITH AN ADDITIONAL BYTE
C          THAT IS ZEROED OUT.

           IAD4 = LEN0+LEN1+LEN2+LEN3
           IAD5 = IAD4+LEN4
           IBIT = IAD4*8
           LEN4 = LEN4+1
           CALL PKB(LEN4,24,MGWA,IBIT)
           IBIT = IAD5*8
           CALL PKB(0,8,MGWA,IBIT)
           MBYT = MBYT+1
        ENDIF
      ENDIF

C  WRITE SECTION 0 BYTE COUNT AND SECTION 5
C  ----------------------------------------

      IBIT = 0
      CALL PKC(BUFR, 4,MGWA,IBIT)
      CALL PKB(MBYT,24,MGWA,IBIT)

      KBIT = (MBYT-4)*8
      CALL PKC(SEVN, 4,MGWA,KBIT)

C  ZERO OUT THE EXTRA BYTES WHICH WILL BE WRITTEN
C  ----------------------------------------------

C     I.E. SINCE THE BUFR MESSAGE IS STORED WITHIN THE INTEGER ARRAY
C     MGWA(*) (RATHER THAN WITHIN A CHARACTER ARRAY), WE NEED TO MAKE
C     SURE THAT THE "7777" IS FOLLOWED BY ZEROED-OUT BYTES UP TO THE
C     BOUNDARY OF THE LAST MACHINE WORD THAT WILL BE WRITTEN OUT.

      CALL PADMSG(MGWA,MXMSGLD4,NPBYT)

C  WRITE THE MESSAGE PLUS PADDING TO A WORD BOUNDARY IF NULL(LUN) = 0
C  ------------------------------------------------------------------

      MWRD = NMWRD(MGWA)
      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(NULL(LUN).EQ.0) THEN
         CALL BLOCKS(MGWA,MWRD)
         CALL CWRBUFR_C(LUN,MGWA,MWRD)
      ENDIF

      IF(IPRT.GE.2) THEN
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
      WRITE ( UNIT=ERRSTR, FMT='(A,I4,A,I7)')
     .  'BUFRLIB: MSGWRT: LUNIT =', LUNIT, ', BYTES =', MBYT+NPBYT
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
      CALL ERRWRT(' ')
      ENDIF

C  SAVE A MEMORY COPY OF THIS MESSAGE, UNLESS IT'S A DX MESSAGE
C  ------------------------------------------------------------

      IF(IDXMSG(MGWA).NE.1) THEN

C        STORE A COPY OF THIS MESSAGE WITHIN MODULE BUFRMG,
C        FOR POSSIBLE LATER RETRIEVAL DURING A FUTURE CALL TO
C        SUBROUTINE WRITSA.

         MSGLEN(LUN) = MWRD
         DO I=1,MSGLEN(LUN)
           MSGTXT(I,LUN) = MGWA(I)
         ENDDO
      ENDIF

C  EXITS
C  -----

      RETURN
901   CALL BORT
     . ('BUFRLIB: MSGWRT - LENGTH OF SECTION 1 IS NOT A MULTIPLE OF 2')
902   CALL BORT
     . ('BUFRLIB: MSGWRT - LENGTH OF SECTION 2 IS NOT A MULTIPLE OF 2')
903   CALL BORT
     . ('BUFRLIB: MSGWRT - LENGTH OF SECTION 3 IS NOT A MULTIPLE OF 2')
      END
