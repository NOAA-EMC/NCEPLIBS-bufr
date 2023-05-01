C> @file
C> @brief Copy unique elements of a data subset.
C>
C> @author Woollen @date 1994-01-06

C> Copy unique elements of a data subset.
C>
C> This subroutine makes one copy of each unique element in an input
C> subset buffer into the identical mnemonic slot in the output subset
C> buffer.
C>
C> Before this subroutine is called:
C> - The input file must be opened for input with openbf().
C> - a message must be read, as with readmg().
C> - a subset of data loaded into memory, as with readsb().
C> - the output file must be opened for output with openbf().
C> - a message must be created in the output file, as with openmg().
C>
C> After this subroutine is called, writsb() must be called on the output
C> file to write the subset to file.
C>
C> @param[in] LUBIN - integer: fortran logical unit number for input BUFR
C> file.
C> @param[in] LUBOT - integer: fortran logical unit number for output
C> BUFR file.
C>
C> @author Woollen @date 1994-01-06
      RECURSIVE SUBROUTINE UFBCUP(LUBIN,LUBOT)

      USE MODV_IM8B

      USE MODA_USRINT
      USE MODA_MSGCWD
      USE MODA_TABLES
      USE MODA_IVTTMP

      CHARACTER*10 TAGO

C----------------------------------------------------------------------
C----------------------------------------------------------------------

C  CHECK FOR I8 INTEGERS
C  ---------------------

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84(LUBIN,MY_LUBIN,1)
         CALL X84(LUBOT,MY_LUBOT,1)
         CALL UFBCUP(MY_LUBIN,MY_LUBOT)

         IM8B=.TRUE.
         RETURN
      ENDIF

C  CHECK THE FILE STATUSES AND I-NODE
C  ----------------------------------

      CALL STATUS(LUBIN,LUI,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IL.GT.0) GOTO 901
      IF(IM.EQ.0) GOTO 902
      IF(INODE(LUI).NE.INV(1,LUI)) GOTO 903

      CALL STATUS(LUBOT,LUO,IL,IM)
      IF(IL.EQ.0) GOTO 904
      IF(IL.LT.0) GOTO 905
      IF(IM.EQ.0) GOTO 906

C  MAKE A LIST OF UNIQUE TAGS IN INPUT BUFFER
C  ------------------------------------------

      NTAG = 0

      DO 5 NI=1,NVAL(LUI)
      NIN = INV(NI,LUI)
      IF(ITP(NIN).GE.2) THEN
         DO NV=1,NTAG
         IF(TTMP(NV).EQ.TAG(NIN)) GOTO 5
         ENDDO
         NTAG = NTAG+1
         ITMP(NTAG) = NI
         TTMP(NTAG) = TAG(NIN)
      ENDIF
5     ENDDO

      IF(NTAG.EQ.0) GOTO 907

C  GIVEN A LIST MAKE ONE COPY OF COMMON ELEMENTS TO OUTPUT BUFFER
C  --------------------------------------------------------------

      DO 10 NV=1,NTAG
      NI = ITMP(NV)
      DO NO=1,NVAL(LUO)
      TAGO = TAG(INV(NO,LUO))
      IF(TTMP(NV).EQ.TAGO) THEN
         VAL(NO,LUO) = VAL(NI,LUI)
         GOTO 10
      ENDIF
      ENDDO
10    ENDDO

C  EXITS
C  -----

      RETURN
900   CALL BORT('BUFRLIB: UFBCUP - INPUT BUFR FILE IS CLOSED, IT '//
     . 'MUST BE OPEN FOR INPUT')
901   CALL BORT('BUFRLIB: UFBCUP - INPUT BUFR FILE IS OPEN FOR '//
     . 'OUTPUT, IT MUST BE OPEN FOR INPUT')
902   CALL BORT('BUFRLIB: UFBCUP - A MESSAGE MUST BE OPEN IN INPUT '//
     . 'BUFR FILE, NONE ARE')
903   CALL BORT('BUFRLIB: UFBCUP - LOCATION OF INTERNAL TABLE FOR '//
     . 'INPUT BUFR FILE DOES NOT AGREE WITH EXPECTED LOCATION IN '//
     . 'INTERNAL SUBSET ARRAY')
904   CALL BORT('BUFRLIB: UFBCUP - OUTPUT BUFR FILE IS CLOSED, IT '//
     . 'MUST BE OPEN FOR OUTPUT')
905   CALL BORT('BUFRLIB: UFBCUP - OUTPUT BUFR FILE IS OPEN FOR '//
     . 'INPUT, IT MUST BE OPEN FOR OUTPUT')
906   CALL BORT('BUFRLIB: UFBCUP - A MESSAGE MUST BE OPEN IN OUTPUT '//
     . 'BUFR FILE, NONE ARE')
907   CALL BORT('BUFRLIB: UFBCUP - THERE ARE NO ELEMENTS (TAGS) IN '//
     . 'INPUT SUBSET BUFFER')
      END
