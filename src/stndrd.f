C> @file
C> @brief Standardize a BUFR message.

C> This subroutine performs the same function as subroutine stdmsg(),
C> except that it operates on a BUFR message passed in via a memory array
C> and returns its output via a separate memory array,
C> whereas stdmsg() operates on BUFR messages stored internally
C> within the software.
C> 
C> @author J. Ator
C> @date 2004-08-18
C>
C> @param[in] LUNIT    - integer: Fortran logical unit number for
C>                       BUFR file
C> @param[in] MSGIN    - integer(*): BUFR message
C> @param[in] LMSGOT   - integer: Dimensioned size (in integers) of
C>                       MSGOT; used by the subroutine to ensure that
C>                       it doesn't overflow the MSGOT array
C> @param[out] MSGOT   - integer(*): Standardized copy of MSGIN
C>
C> @remarks
C> - MSGIN and MSGOT must be separate arrays.
C> - Standardized messages are usually longer in length than their
C> non-standard counterparts, so it's usually a good idea to allow
C> for extra space when allocating MSGOT within the application program.
C> 
C> <b>Program history log:</b>
C> - 2004-08-18  J. Ator    -- Original author
C> - 2005-11-29  J. Ator    -- Use getlens() and iupbs01(); ensure that
C>                           byte 4 of Section 4 is zeroed out in MSGOT;
C>                           check edition number of BUFR message before 
C>                           padding to an even byte count
C> - 2009-03-23  J. Ator    -- Use iupbs3() and nemtbax(); don't assume
C>                           that compressed messages are already fully
C>                           standardized within Section 3
C> - 2014-02-04  J. Ator    -- Account for subsets with byte count > 65530
C> - 2020-07-16  J. Ator    -- Fix bug in ISLEN computation when NSUB = 1
C>
      SUBROUTINE STNDRD(LUNIT,MSGIN,LMSGOT,MSGOT)

      USE MODV_MAXNC

      DIMENSION ICD(MAXNC)

      COMMON /HRDWRD/ NBYTW,NBITW,IORD(8)

      DIMENSION MSGIN(*),MSGOT(*)

      CHARACTER*128 BORT_STR
      CHARACTER*8   SUBSET
      CHARACTER*4   SEVN
      CHARACTER*1   TAB

      LOGICAL FOUND

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  LUNIT MUST POINT TO AN OPEN BUFR FILE
C  -------------------------------------

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900

C  IDENTIFY THE SECTION LENGTHS AND ADDRESSES IN MSGIN
C  ---------------------------------------------------

      CALL GETLENS(MSGIN,5,LEN0,LEN1,LEN2,LEN3,LEN4,LEN5)

      IAD3 = LEN0+LEN1+LEN2
      IAD4 = IAD3+LEN3

      LENN = LEN0+LEN1+LEN2+LEN3+LEN4+LEN5

      LENM = IUPBS01(MSGIN,'LENM')

      IF(LENN.NE.LENM) GOTO 901

      MBIT = (LENN-4)*8
      CALL UPC(SEVN,4,MSGIN,MBIT,.TRUE.)
      IF(SEVN.NE.'7777') GOTO 902

C  COPY SECTIONS 0 THROUGH PART OF SECTION 3 INTO MSGOT
C  ----------------------------------------------------

      MXBYTO = (LMSGOT*NBYTW) - 8

      LBYTO = IAD3+7
      IF(LBYTO.GT.MXBYTO) GOTO 905
      CALL MVB(MSGIN,1,MSGOT,1,LBYTO)

C  REWRITE NEW SECTION 3 IN A "STANDARD" FORM
C  ------------------------------------------

C     LOCATE THE TOP-LEVEL TABLE A DESCRIPTOR

      FOUND = .FALSE.
      II = 10
      DO WHILE ((.NOT.FOUND).AND.(II.GE.8))
          ISUB = IUPB(MSGIN,IAD3+II,16)
          CALL NUMTAB(LUN,ISUB,SUBSET,TAB,ITAB)
          IF((ITAB.NE.0).AND.(TAB.EQ.'D')) THEN
              CALL NEMTBAX(LUN,SUBSET,MTYP,MSBT,INOD)
              IF(INOD.NE.0) FOUND = .TRUE.
          ENDIF
          II = II - 2
      ENDDO
      IF(.NOT.FOUND) GOTO 903

      IF (ISTDESC(ISUB).EQ.0) THEN

C         ISUB IS A NON-STANDARD TABLE A DESCRIPTOR AND NEEDS
C         TO BE EXPANDED INTO AN EQUIVALENT STANDARD SEQUENCE  

          CALL RESTD(LUN,ISUB,NCD,ICD)
      ELSE

C         ISUB IS ALREADY A STANDARD DESCRIPTOR, SO JUST COPY
C         IT "AS IS" INTO THE NEW SECTION 3 (I.E. NO EXPANSION
C         IS NECESSARY!)

          NCD = 1
          ICD(NCD) = ISUB
      ENDIF

C     USE THE EDITION NUMBER TO DETERMINE THE LENGTH OF THE
C     NEW SECTION 3

      LEN3 = 7+(NCD*2)
      IBEN = IUPBS01(MSGIN,'BEN')
      IF(IBEN.LT.4) THEN
          LEN3 = LEN3+1
      ENDIF
      LBYTO = LBYTO + LEN3 - 7
      IF(LBYTO.GT.MXBYTO) GOTO 905

C     STORE THE DESCRIPTORS INTO THE NEW SECTION 3

      IBIT = (IAD3+7)*8
      DO N=1,NCD
          CALL PKB(ICD(N),16,MSGOT,IBIT)
      ENDDO

C     DEPENDING ON THE EDITION NUMBER, PAD OUT THE NEW SECTION 3 WITH AN
C     ADDITIONAL ZEROED-OUT BYTE IN ORDER TO ENSURE AN EVEN BYTE COUNT

      IF(IBEN.LT.4) THEN
          CALL PKB(0,8,MSGOT,IBIT)
      ENDIF

C     STORE THE LENGTH OF THE NEW SECTION 3

      IBIT = IAD3*8
      CALL PKB(LEN3,24,MSGOT,IBIT)

C  NOW THE TRICKY PART - NEW SECTION 4
C  -----------------------------------

      IF(IUPBS3(MSGIN,'ICMP').EQ.1) THEN

C         THE DATA IN SECTION 4 IS COMPRESSED AND IS THEREFORE ALREADY
C         STANDARDIZED, SO COPY IT "AS IS" INTO THE NEW SECTION 4

          IF((LBYTO+LEN4+4).GT.MXBYTO) GOTO 905

          CALL MVB(MSGIN,IAD4+1,MSGOT,LBYTO+1,LEN4)

          JBIT = (LBYTO+LEN4)*8

      ELSE

          NAD4 = IAD3+LEN3

          IBIT = (IAD4+4)*8
          JBIT = (NAD4+4)*8

          LBYTO = LBYTO + 4

C         COPY THE SUBSETS, MINUS THE BYTE COUNTERS AND BIT PADS, INTO
C         THE NEW SECTION 4

          NSUB = IUPBS3(MSGIN,'NSUB')

          DO 10 I=1,NSUB
              CALL UPB(LSUB,16,MSGIN,IBIT)
              IF(NSUB.GT.1) THEN

C                 USE THE BYTE COUNTER TO COPY THIS SUBSET

                  ISLEN = LSUB-2
              ELSE

C                 THIS IS THE ONLY SUBSET IN THE MESSAGE, AND IT COULD
C                 POSSIBLY BE AN OVERLARGE (> 65530 BYTES) SUBSET, IN
C                 WHICH CASE WE CAN'T RELY ON THE VALUE STORED IN THE
C                 BYTE COUNTER.  EITHER WAY, WE DON'T REALLY NEED IT.

                  ISLEN = IAD4+LEN4-(IBIT/8)
                  IF (MOD(LEN4,2).EQ.0) ISLEN = ISLEN - 1
              ENDIF
              DO L=1,ISLEN
                  CALL UPB(NVAL,8,MSGIN,IBIT)
                  LBYTO = LBYTO + 1
                  IF(LBYTO.GT.MXBYTO) GOTO 905
                  CALL PKB(NVAL,8,MSGOT,JBIT)
              ENDDO
              DO K=1,8
                  KBIT = IBIT-K-8
                  CALL UPB(KVAL,8,MSGIN,KBIT)
                  IF(KVAL.EQ.K) THEN
                     JBIT = JBIT-K-8
                     GOTO 10
                  ENDIF
              ENDDO
              GOTO 904
10        ENDDO

C         FROM THIS POINT ON, WE WILL NEED (AT MOST) 6 MORE BYTES OF
C         SPACE WITHIN MSGOT IN ORDER TO BE ABLE TO STORE THE ENTIRE
C         STANDARDIZED MESSAGE (I.E. WE WILL NEED (AT MOST) 2 MORE
C         ZEROED-OUT BYTES IN SECTION 4 PLUS THE 4 BYTES '7777' IN
C         SECTION 5), SO DO A FINAL MSGOT OVERFLOW CHECK NOW.

          IF(LBYTO+6.GT.MXBYTO) GOTO 905

C         PAD THE NEW SECTION 4 WITH ZEROES UP TO THE NEXT WHOLE BYTE
C         BOUNDARY.

          DO WHILE(.NOT.(MOD(JBIT,8).EQ.0))
             CALL PKB(0,1,MSGOT,JBIT)
          ENDDO

C         DEPENDING ON THE EDITION NUMBER, WE MAY NEED TO FURTHER PAD
C         THE NEW SECTION 4 WITH AN ADDITIONAL ZEROED-OUT BYTE IN ORDER
C         TO ENSURE THAT THE PADDING IS UP TO AN EVEN BYTE BOUNDARY.

          IF( (IBEN.LT.4) .AND. (MOD(JBIT/8,2).NE.0) ) THEN
             CALL PKB(0,8,MSGOT,JBIT)
          ENDIF

          IBIT = NAD4*8
          LEN4 = JBIT/8 - NAD4
          CALL PKB(LEN4,24,MSGOT,IBIT)
          CALL PKB(0,8,MSGOT,IBIT)
      ENDIF

C  FINISH THE NEW MESSAGE WITH AN UPDATED SECTION 0 BYTE COUNT
C  -----------------------------------------------------------

      IBIT = 32
      LENN = LEN0+LEN1+LEN2+LEN3+LEN4+LEN5
      CALL PKB(LENN,24,MSGOT,IBIT)

      CALL PKC('7777',4,MSGOT,JBIT)

C  EXITS
C  -----

      RETURN
900   CALL BORT('BUFRLIB: STNDRD - BUFR FILE IS CLOSED, IT MUST BE'//
     . ' OPEN')
901   WRITE(BORT_STR,'("BUFRLIB: STNDRD - INPUT MESSAGE LENGTH FROM'//
     . ' SECTION 0",I6," DOES NOT EQUAL SUM OF ALL INDIVIDUAL SECTION'//
     . ' LENGTHS (",I6,")")') LENM,LENN
      CALL BORT(BORT_STR)
902   WRITE(BORT_STR,'("BUFRLIB: STNDRD - INPUT MESSAGE DOES NOT '//
     . 'END WITH ""7777"" (ENDS WITH ",A)') SEVN
      CALL BORT(BORT_STR)
903   CALL BORT('BUFRLIB: STNDRD - TABLE A SUBSET DESCRIPTOR '//
     . 'NOT FOUND')
904   CALL BORT('BUFRLIB: STNDRD - BIT MISMATCH COPYING SECTION 4 '//
     . 'FROM INPUT TO OUTPUT (STANDARD) MESSAGE')
905   CALL BORT('BUFRLIB: STNDRD - OVERFLOW OF OUTPUT (STANDARD) '//
     . 'MESSAGE ARRAY; TRY A LARGER DIMENSION FOR THIS ARRAY')
      END
