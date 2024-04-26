C> @file
C> @brief Write a compressed BUFR data subset.
C>
C> @author Woollen @date 2002-05-14

C> Pack up the current subset within memory
C> (array ibay in module @ref moda_bitbuf), storing it for compression.
C> Then try to add it to the compressed BUFR message that is
C> currently open within memory for ABS(LUNIX) (array mgwa). If the
C> subset will not fit into the currently open message, then that
C> compressed message is flushed to lunix and a new one is created in
C> order to hold the current subset (still stored for compression).
C>
C> This subroutine performs functions similar to NCEPLIBS-bufr
C> subroutine msgupd() except that it acts on compressed bufr messages.
C>
C> @param[in] LUNIX - integer: absolute value is Fortran logical unit
C> number for BUFR file (if LUNIX is less than zero, this is a "flush"
C> call and the buffer must be cleared out)
C>
C> @author Woollen @date 2002-05-14

      SUBROUTINE WRCMPS(LUNIX)

      use modv_vars, only: mxcdv, mxcsb

      use moda_usrint
      use moda_msgcwd
      use moda_bitbuf
      use moda_mgwa
      use moda_tables
      use moda_comprx
      use moda_comprs
      use moda_s01cm

      COMMON /MAXCMP/ MAXCMB,MAXROW,MAXCOL,NCMSGS,NCSUBS,NCBYTS

      CHARACTER*128 BORT_STR
      CHARACTER*8   SUBSET
      CHARACTER*1   CZERO

      LOGICAL MSGFULL

C     NOTE THE FOLLOWING LOGICAL FLAGS:
C         FIRST - KEEPS TRACK OF WHETHER THE CURRENT SUBSET IS THE
C                 FIRST SUBSET OF A NEW MESSAGE
C         FLUSH - KEEPS TRACK OF WHETHER THIS SUBROUTINE WAS CALLED
C                 WITH LUNIX < 0 IN ORDER TO FORCIBLY FLUSH ANY
C                 PARTIALLY-COMPLETED MESSAGE WITHIN MEMORY (PRESUMABLY
C                 IMMEDIATELY PRIOR TO EXITING THE CALLING PROGRAM!)
C         WRIT1 - KEEPS TRACK OF WHETHER THE CURRENT MESSAGE NEEDS
C                 TO BE WRITTEN OUT

      LOGICAL       FIRST,KMISS,EDGE4

      DATA      FIRST /.TRUE./

      SAVE      FIRST,IBYT,JBIT,SUBSET,EDGE4

C-----------------------------------------------------------------------
      RLN2 = 1./LOG(2.)
C-----------------------------------------------------------------------

C  GET THE UNIT AND SUBSET TAG
C  ---------------------------

      LUNIT = ABS(LUNIX)
      CALL STATUS(LUNIT,LUN,IL,IM)

C  IF THIS IS A "FIRST" CALL, THEN INITIALIZE SOME VALUES IN
C  ORDER TO PREPARE FOR THE CREATION OF A NEW COMPRESSED BUFR
C  MESSAGE FOR OUTPUT.

  1   IF(FIRST) THEN
         KBYT = 0
         NCOL = 0
         LUNC = LUN
         NROW = NVAL(LUN)
         SUBSET = TAG(INODE(LUN))(1:8)
         FIRST = .FALSE.
         FLUSH = .FALSE.
         WRIT1 = .FALSE.

C        THIS CALL TO CMSGINI IS DONE SOLELY IN ORDER TO DETERMINE
C        HOW MANY BYTES (KBYT) WILL BE TAKEN UP IN A MESSAGE BY
C        THE INFORMATION IN SECTIONS 0, 1, 2 AND 3.  THIS WILL
C        ALLOW US TO KNOW HOW MANY COMPRESSED DATA SUBSETS WILL
C        FIT INTO SECTION 4 WITHOUT OVERFLOWING MAXCMB.  LATER ON,
C        A SEPARATE CALL TO CMSGINI WILL BE DONE TO ACTUALLY
C        INITIALIZE SECTIONS 0, 1, 2 AND 3 OF THE FINAL COMPRESSED
C        BUFR MESSAGE THAT WILL BE WRITTEN OUT.

         CALL CMSGINI(LUN,MBAY(1,LUN),SUBSET,IDATE(LUN),NCOL,KBYT)

C        CHECK THE EDITION NUMBER OF THE BUFR MESSAGE TO BE CREATED

         EDGE4 = .FALSE.
         IF(NS01V.GT.0) THEN
           II = 1
           DO WHILE ( (.NOT.EDGE4) .AND. (II.LE.NS01V) )
             IF( (CMNEM(II).EQ.'BEN') .AND. (IVMNEM(II).GE.4) ) THEN
               EDGE4 = .TRUE.
             ELSE
               II = II+1
             ENDIF
           ENDDO
         ENDIF

      ENDIF

      IF(LUN.NE.LUNC) GOTO 900

C  IF THIS IS A "FLUSH" CALL, THEN CLEAR OUT THE BUFFER (NOTE THAT
C  THERE IS NO CURRENT SUBSET TO BE STORED!) AND PREPARE TO WRITE
C  THE FINAL COMPRESSED BUFR MESSAGE.

      IF(LUNIX.LT.0) THEN
         IF(NCOL.EQ.0) GOTO 100
         IF(NCOL.GT.0) THEN
            FLUSH = .TRUE.
            WRIT1 = .TRUE.
            ICOL = 1
            GOTO 20
         ENDIF
      ENDIF

C  CHECK ON SOME OTHER POSSIBLY PROBLEMATIC SITUATIONS
C  ---------------------------------------------------

      IF(NCOL+1.GT.MXCSB) THEN
         GOTO 50
      ELSEIF(NVAL(LUN).NE.NROW) THEN
         WRIT1 = .TRUE.
         ICOL = 1
         GOTO 20
      ELSEIF(NVAL(LUN).GT.MXCDV) THEN
         GOTO 901
      else if (ncol.gt.0) then
C        Confirm that all of the nodes are the same as in the previous
C        subset for this same BUFR message.  If not, then there may be
C        different nested replication sequences activated in the current
C        subset vs. in the previous subset, even though the total number
C        of nodes is the same.
         do i = 1, nval(lun)
           if ( inv(i,lun) .ne. jlnode(i) ) then
             WRIT1 = .TRUE.
             ICOL = 1
             GOTO 20
           endif
         enddo
      ENDIF

C  STORE THE NEXT SUBSET FOR COMPRESSION
C  -------------------------------------

C     WILL THE CURRENT SUBSET FIT INTO THE CURRENT MESSAGE?
C     (UNFORTUNATELY, THE ONLY WAY TO FIND OUT IS TO ACTUALLY
C     RE-DO THE COMPRESSION BY RE-COMPUTING ALL OF THE LOCAL
C     REFERENCE VALUES, INCREMENTS, ETC.)

      NCOL = NCOL+1
      ICOL = NCOL
      IBIT = 16
      DO I=1,NVAL(LUN)
      NODE = INV(I,LUN)
      jlnode(i) = node
      ITYP(I) = ITP(NODE)
      IWID(I) = IBT(NODE)
      IF(ITYP(I).EQ.1.OR.ITYP(I).EQ.2) THEN
         CALL UP8(MATX(I,NCOL),IBT(NODE),IBAY,IBIT)
      ELSEIF(ITYP(I).EQ.3) THEN
         CALL UPC(CATX(I,NCOL),IBT(NODE)/8,IBAY,IBIT,.TRUE.)
      ENDIF
      ENDDO

C  COMPUTE THE MIN,MAX,WIDTH FOR EACH ROW - ACCUMULATE LENGTH
C  ----------------------------------------------------------

C     LDATA WILL HOLD THE LENGTH IN BITS OF THE COMPRESSED DATA
C     (I.E. THE SUM TOTAL FOR ALL DATA VALUES FOR ALL SUBSETS
C     IN THE MESSAGE)

 20   LDATA = 0
      IF(NCOL.LE.0) GOTO 902
      DO I=1,NROW
      IF(ITYP(I).EQ.1 .OR. ITYP(I).EQ.2) THEN

C        ROW I OF THE COMPRESSION MATRIX CONTAINS NUMERIC VALUES,
C        SO KMIS(I) WILL STORE:
C          .FALSE. IF ALL SUCH VALUES ARE NON-"MISSING"
C          .TRUE. OTHERWISE

         IMISS = 2_8**IWID(I)-1
         IF(ICOL.EQ.1) THEN
            KMIN(I) = IMISS
            KMAX(I) = 0
            KMIS(I) = .FALSE.
         ENDIF
         DO J=ICOL,NCOL
         IF(MATX(I,J).LT.IMISS) THEN
            KMIN(I) = MIN(KMIN(I),MATX(I,J))
            KMAX(I) = MAX(KMAX(I),MATX(I,J))
         ELSE
            KMIS(I) = .TRUE.
         ENDIF
         ENDDO
         KMISS = KMIS(I).AND.KMIN(I).LT.IMISS
         RANGE = REAL(MAX(1,KMAX(I)-KMIN(I)+1))
         IF(ITYP(I).EQ.2.AND.(RANGE.GT.1..OR.KMISS)) THEN

C           THE DATA VALUES IN ROW I OF THE COMPRESSION MATRIX
C           ARE NUMERIC VALUES THAT ARE NOT ALL IDENTICAL.
C           COMPUTE THE NUMBER OF BITS NEEDED TO HOLD THE
C           LARGEST OF THE INCREMENTS.

            KBIT(I) = NINT(LOG(RANGE)*RLN2)
            IF(2**KBIT(I)-1.LE.RANGE) KBIT(I) = KBIT(I)+1

C           HOWEVER, UNDER NO CIRCUMSTANCES SHOULD THIS NUMBER
C           EVER EXCEED THE WIDTH OF THE ORIGINAL UNDERLYING
C           DESCRIPTOR!

            IF(KBIT(I).GT.IWID(I)) KBIT(I) = IWID(I)
         ELSE

C           THE DATA VALUES IN ROW I OF THE COMPRESSION MATRIX
C           ARE NUMERIC VALUES THAT ARE ALL IDENTICAL, SO THE
C           INCREMENTS WILL BE OMITTED FROM THE MESSAGE.

            KBIT(I) = 0
         ENDIF
         LDATA = LDATA + IWID(I) + 6 + NCOL*KBIT(I)
      ELSEIF(ITYP(I).EQ.3) THEN

C        ROW I OF THE COMPRESSION MATRIX CONTAINS CHARACTER VALUES,
C        SO KMIS(I) WILL STORE:
C          .FALSE. IF ALL SUCH VALUES ARE IDENTICAL
C          .TRUE. OTHERWISE

         IF(ICOL.EQ.1) THEN
            CSTR(I) = CATX(I,1)
            KMIS(I) = .FALSE.
         ENDIF
         DO J=ICOL,NCOL
            IF ( (.NOT.KMIS(I)) .AND. (CSTR(I).NE.CATX(I,J)) ) THEN
               KMIS(I) = .TRUE.
            ENDIF
         ENDDO
         IF (KMIS(I)) THEN

C           THE DATA VALUES IN ROW I OF THE COMPRESSION MATRIX
C           ARE CHARACTER VALUES THAT ARE NOT ALL IDENTICAL.

            KBIT(I) = IWID(I)
         ELSE

C           THE DATA VALUES IN ROW I OF THE COMPRESSION MATRIX
C           ARE CHARACTER VALUES THAT ARE ALL IDENTICAL, SO THE
C           INCREMENTS WILL BE OMITTED FROM THE MESSAGE.

            KBIT(I) = 0
         ENDIF
         LDATA = LDATA + IWID(I) + 6 + NCOL*KBIT(I)
      ENDIF
      ENDDO

C  ROUND DATA LENGTH UP TO A WHOLE BYTE COUNT
C  ------------------------------------------

      IBYT = (LDATA+8-MOD(LDATA,8))/8

C     DEPENDING ON THE EDITION NUMBER OF THE MESSAGE, WE NEED TO ENSURE
C     THAT WE ROUND TO AN EVEN BYTE COUNT

      IF( (.NOT.EDGE4) .AND. (MOD(IBYT,2).NE.0) ) IBYT = IBYT+1

      JBIT = IBYT*8-LDATA

C  CHECK ON COMPRESSED MESSAGE LENGTH, EITHER WRITE/RESTORE OR RETURN
C  ------------------------------------------------------------------

      IF(MSGFULL(IBYT,KBYT,MAXCMB)) THEN

C        THE CURRENT SUBSET WILL NOT FIT INTO THE CURRENT MESSAGE.
C        SET THE FLAG TO INDICATE THAT A MESSAGE WRITE IS NEEDED,
C        THEN GO BACK AND RE-COMPRESS THE SECTION 4 DATA FOR THIS
C        MESSAGE WHILE *EXCLUDING* THE DATA FOR THE CURRENT SUBSET
C        (WHICH WILL BE HELD AND STORED AS THE FIRST SUBSET OF A
C        NEW MESSAGE AFTER WRITING THE CURRENT MESSAGE!).

         WRIT1 = .TRUE.
         NCOL = NCOL-1
         ICOL = 1
         GOTO 20
      ELSEIF(.NOT.WRIT1) THEN

C        ADD THE CURRENT SUBSET TO THE CURRENT MESSAGE AND RETURN.

         CALL USRTPL(LUN,1,1)
         NSUB(LUN) = -NCOL
         GOTO 100
      ENDIF

C  WRITE THE COMPLETE COMPRESSED MESSAGE
C  -------------------------------------

C     NOW IT IS TIME TO DO THE "REAL" CALL TO CMSGINI TO ACTUALLY
C     INITIALIZE SECTIONS 0, 1, 2 AND 3 OF THE FINAL COMPRESSED
C     BUFR MESSAGE THAT WILL BE WRITTEN OUT.

 50   CALL CMSGINI(LUN,MGWA,SUBSET,IDATE(LUN),NCOL,IBYT)

C     NOW ADD THE SECTION 4 DATA.

      IBIT = IBYT*8
      DO I=1,NROW
      IF(ITYP(I).EQ.1.OR.ITYP(I).EQ.2) THEN
         CALL PKB8(KMIN(I),IWID(I),MGWA,IBIT)
         CALL PKB(KBIT(I),      6,MGWA,IBIT)
         IF(KBIT(I).GT.0) THEN
            DO J=1,NCOL
            IF(MATX(I,J).LT.2_8**IWID(I)-1) THEN
               INCR = MATX(I,J)-KMIN(I)
            ELSE
               INCR = 2_8**KBIT(I)-1
            ENDIF
            CALL PKB8(INCR,KBIT(I),MGWA,IBIT)
            ENDDO
         ENDIF
      ELSEIF(ITYP(I).EQ.3) THEN
         NCHR = IWID(I)/8
         IF(KBIT(I).GT.0) THEN
            CALL IPKM(CZERO,1,0)
            DO J=1,NCHR
               CALL PKC(CZERO,1,MGWA,IBIT)
            ENDDO
            CALL PKB(NCHR,      6,MGWA,IBIT)
            DO J=1,NCOL
               CALL PKC(CATX(I,J),NCHR,MGWA,IBIT)
            ENDDO
         ELSE
            CALL PKC(CSTR(I),NCHR,MGWA,IBIT)
            CALL PKB(      0,   6,MGWA,IBIT)
         ENDIF
      ENDIF
      ENDDO

C  FILL IN THE END OF THE MESSAGE
C  ------------------------------

C     PAD THE END OF SECTION 4 WITH ZEROES UP TO THE NECESSARY
C     BYTE COUNT.

      CALL PKB(     0,JBIT,MGWA,IBIT)

C     ADD SECTION 5.

      CALL PKC('7777',   4,MGWA,IBIT)

C  SEE THAT THE MESSAGE BYTE COUNTERS AGREE THEN WRITE A MESSAGE
C  -------------------------------------------------------------

      IF(MOD(IBIT,8).NE.0) GOTO 904
      LBYT = IUPBS01(MGWA,'LENM')
      NBYT = IBIT/8
      IF(NBYT.NE.LBYT) GOTO 905

      CALL MSGWRT(LUNIT,MGWA,NBYT)

      MAXROW = MAX(MAXROW,NROW)
      MAXCOL = MAX(MAXCOL,NCOL)
      NCMSGS = NCMSGS+1
      NCSUBS = NCSUBS+NCOL
      NCBYTS = NCBYTS+NBYT

C  RESET
C  -----

C     NOW, UNLESS THIS WAS A "FLUSH" CALL TO THIS SUBROUTINE, GO BACK
C     AND INITIALIZE A NEW MESSAGE TO HOLD THE CURRENT SUBSET THAT WE
C     WERE NOT ABLE TO FIT INTO THE MESSAGE THAT WAS JUST WRITTEN OUT.

      FIRST = .TRUE.
      IF(.NOT.FLUSH) GOTO 1

C  EXITS
C  -----

100   RETURN
900   WRITE(BORT_STR,'("BUFRLIB: WRCMPS - FILE ID FOR THIS '//
     . 'CALL (",I3,") .NE. FILE ID FOR INITIAL CALL (",I3,")'//
     . ' - UNIT NUMBER NOW IS",I4)') LUN,LUNC,LUNIX
      CALL BORT(BORT_STR)
901   WRITE(BORT_STR,'("BUFRLIB: WRCMPS - NO. OF ELEMENTS IN THE '//
     . 'SUBSET (",I6,") .GT. THE NO. OF ROWS ALLOCATED FOR THE '//
     . 'COMPRESSION MATRIX (",I6,")")') NVAL(LUN),MXCDV
      CALL BORT(BORT_STR)
902   WRITE(BORT_STR,'("BUFRLIB: WRCMPS - NO. OF COLUMNS CALCULATED '//
     . 'FOR COMPRESSION MAXRIX IS .LE. 0 (=",I6,")")') NCOL
      CALL BORT(BORT_STR)
904   CALL BORT('BUFRLIB: WRCMPS - THE NUMBER OF BITS IN THE '//
     . 'COMPRESSED BUFR MSG IS NOT A MULTIPLE OF 8 - MSG MUST END ON '//
     . ' A BYTE BOUNDARY')
905   WRITE(BORT_STR,'("BUFRLIB: WRCMPS - OUTPUT MESSAGE LENGTH FROM '//
     . 'SECTION 0",I6," DOES NOT EQUAL FINAL PACKED MESSAGE LENGTH ("'//
     .',I6,")")') LBYT,NBYT
      CALL BORT(BORT_STR)
      END
