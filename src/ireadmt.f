C> @file
C> @brief Check whether master BUFR tables need to be read from the
C> local file system
C>
C> @author J. Ator @date 2009-03-23

C> This function checks the most recent BUFR message that was read
C> via a call to one of the
C> [message-reading subroutines](@ref hierarchy) and determines
C> whether the appropriate corresponding BUFR master tables have
C> already been read into internal memory.
C>
C> If not, then it opens the appropriate master BUFR tables on the
C> local file system and then reads the contents into internal
C> memory, clearing any previous master BUFR table information that
C> may have previously been stored there.
C>
C> @param[in]  LUN     -- integer: Internal I/O stream index associated
C>                        with BUFR file
C> @returns ireadmt    -- integer: Flag indicating whether new master
C>                        BUFR tables needed to be read into internal
C>                        memory:
C>                        - 0 = No
C>                        - 1 = Yes
C>
C> Information about the location of master BUFR tables on the
C> local file system is obtained from the most recent call to
C> subroutine mtinfo(), or else from subroutine bfrini() if
C> subroutine mtinfo() was never called, and in which case Fortran
C> logical unit numbers 98 and 99 will be used by this function
C> for opening and reading master BUFR table files.
C>
C> @author J. Ator @date 2009-03-23
        INTEGER FUNCTION IREADMT ( LUN )

        USE MODV_MAXNC
        USE MODV_MAXCD
        USE MODV_MXMTBB
        USE MODV_MXMTBD

        USE MODA_MSTABS
        USE MODA_BITBUF
        USE MODA_RDMTB
        USE MODA_SC3BFR

        COMMON /QUIET/  IPRT
        COMMON /MSTINF/ LUN1, LUN2, LMTD, MTDIR
        COMMON /TABLEF/ CDMF

        CHARACTER*1     CDMF
        CHARACTER*6     CDS3(MAXNC)
        CHARACTER*100   MTDIR
        CHARACTER*128   BORT_STR
        CHARACTER*132   STDFIL,LOCFIL
        LOGICAL         ALLSTD

C*      Initializing the following value ensures that new master tables
C*      are read during the first call to this subroutine.

        DATA    LMT /-99/

        SAVE    LMT, LMTV, LOGCE, LMTVL

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

        IREADMT = 0

C*      Unpack some Section 1 information from the message that was
C*      most recently read.

        IMT  = IUPBS01 ( MBAY(1,LUN), 'BMT' )
        IMTV = IUPBS01 ( MBAY(1,LUN), 'MTV' )
        IOGCE = IUPBS01 ( MBAY(1,LUN), 'OGCE' )
        IMTVL = IUPBS01 ( MBAY(1,LUN), 'MTVL' )

C*      Compare the master table and master table version numbers from
C*      this message to those from the message that was processed during
C*      the previous call to this subroutine.

        IF (  ( IMT .NE. LMT )
     .          .OR.
     .      ( ( IMT .NE. 0 ) .AND. ( IMTV .NE. LMTV ) )
     .          .OR.
     .      ( ( IMT .EQ. 0 ) .AND. ( IMTV .NE. LMTV ) .AND.
     .        ( ( IMTV .GT. 13 ) .OR. ( LMTV .GT. 13 ) ) )  )
     .    THEN

C*        Either the master table number has changed
C*              .OR.
C*        The master table number hasn't changed, but it isn't 0, and
C*        the table version number has changed
C*              .OR.
C*        The master table number hasn't changed and is 0, but the table
C*        version number has changed, and at least one of the table
C*        version numbers (i.e. the current or the previous) is greater
C*        than 13 (which is the last version that was a superset of all
C*        earlier versions of master table 0!)

C*        In any of these cases, we need to read in new tables!

          IREADMT = 1
        ELSE

C*        Unpack the list of Section 3 descriptors from the message and
C*        determine if any of them are local descriptors.

          CALL UPDS3 ( MBAY(1,LUN), MAXNC, CDS3, NCDS3 )
          II = 1
          ALLSTD = .TRUE.
          DO WHILE ( (ALLSTD) .AND. (II.LE.NCDS3) )
            IF ( ISTDESC(IFXY(CDS3(II))) .EQ. 0 ) THEN
              ALLSTD = .FALSE.
            ELSE
              II = II + 1
            ENDIF
          ENDDO

C*        If there was at least one local (i.e. non-standard) descriptor,
C*        and if either the originating center or local table version
C*        number are different than those from the message that was
C*        processed during the previous call to this subroutine, then
C*        we need to read in new tables.

          IF ( ( .NOT. ALLSTD ) .AND.
     +          ( ( IOGCE .NE. LOGCE ) .OR. ( IMTVL .NE. LMTVL ) ) )
     +        IREADMT = 1

        ENDIF

        IF ( IREADMT .EQ. 0 ) RETURN

        LMT  = IMT
        LMTV = IMTV
        LOGCE = IOGCE
        LMTVL = IMTVL

        IF ( IPRT .GE. 2 ) THEN
        CALL ERRWRT(' ')
        CALL ERRWRT('+++++++++++++++++++++++++++++++++++++++++++++++++')
        CALL ERRWRT('BUFRLIB: IREADMT - OPENING/READING MASTER TABLES')
        ENDIF

        IF ( ISC3(LUN) .NE. 0 ) THEN

C*        Locate and open the master Table B files.  There should be one
C*        file of standard descriptors and one file of local descriptors.

          CALL MTFNAM ( IMT, IMTV, IOGCE, IMTVL, 'TableB',
     .                  STDFIL, LOCFIL )
          OPEN ( UNIT = LUN1, FILE = STDFIL, IOSTAT = IER )
          IF ( IER .NE. 0 ) GOTO 900
          OPEN ( UNIT = LUN2, FILE = LOCFIL, IOSTAT = IER )
          IF ( IER .NE. 0 ) GOTO 901

C*        Read the master Table B files.

          CALL RDMTBB ( LUN1, LUN2, MXMTBB,
     .                  IBMT, IBMTV, IBOGCE, IBLTV,
     .                  NMTB, IBFXYN, CBSCL, CBSREF, CBBW,
     .                  CBUNIT, CBMNEM, CMDSCB, CBELEM )

C*        Close the master Table B files.

          CLOSE ( UNIT = LUN1 )
          CLOSE ( UNIT = LUN2 )

C*        Locate and open the master Table D files.  There should be one
C*        file of standard descriptors and one file of local descriptors.

          CALL MTFNAM ( IMT, IMTV, IOGCE, IMTVL, 'TableD',
     .                  STDFIL, LOCFIL )
          OPEN ( UNIT = LUN1, FILE = STDFIL, IOSTAT = IER )
          IF ( IER .NE. 0 ) GOTO 900
          OPEN ( UNIT = LUN2, FILE = LOCFIL, IOSTAT = IER )
          IF ( IER .NE. 0 ) GOTO 901

C*        Read the master Table D files.

          CALL RDMTBD ( LUN1, LUN2, MXMTBD, MAXCD,
     .                  IDMT, IDMTV, IDOGCE, IDLTV,
     .                  NMTD, IDFXYN, CDMNEM, CMDSCD, CDSEQ,
     .                  NDELEM, IEFXYN, CEELEM )
          DO I = 1, NMTD
            DO J = 1, NDELEM(I)
              IDX = ICVIDX ( I-1, J-1, MAXCD ) + 1
              IDEFXY(IDX) = IEFXYN(I,J)
            ENDDO
          ENDDO

C*        Close the master Table D files.

          CLOSE ( UNIT = LUN1 )
          CLOSE ( UNIT = LUN2 )

C*        Copy master table B and D information into internal C arrays.

          CALL CPMSTABS ( NMTB, IBFXYN, CBSCL, CBSREF, CBBW, CBUNIT,
     .                    CBMNEM, CBELEM, NMTD, IDFXYN, CDSEQ, CDMNEM,
     .                    NDELEM, IDEFXY, MAXCD )
        ENDIF

        IF ( CDMF .EQ. 'Y' ) THEN

C*        Locate and open the master code and flag table files.  There
C*        should be one file corresponding to the standard Table B
C*        descriptors, and one file corresponding to the local Table B
C*        descriptors.

          CALL MTFNAM ( IMT, IMTV, IOGCE, IMTVL, 'CodeFlag',
     .                STDFIL, LOCFIL )
          OPEN ( UNIT = LUN1, FILE = STDFIL, IOSTAT = IER )
          IF ( IER .NE. 0 ) GOTO 900
          OPEN ( UNIT = LUN2, FILE = LOCFIL, IOSTAT = IER )
          IF ( IER .NE. 0 ) GOTO 901

C*        Read the master code and flag table files.

          CALL RDMTBF ( LUN1, LUN2 )

C*        Close the master code and flag table files.

          CLOSE ( UNIT = LUN1 )
          CLOSE ( UNIT = LUN2 )

        ENDIF

        IF ( IPRT .GE. 2 ) THEN
        CALL ERRWRT('+++++++++++++++++++++++++++++++++++++++++++++++++')
        CALL ERRWRT(' ')
        ENDIF

        RETURN
900     BORT_STR = 'BUFRLIB: IREADMT - COULD NOT OPEN STANDARD FILE:'
        CALL BORT2(BORT_STR,STDFIL)
901     BORT_STR = 'BUFRLIB: IREADMT - COULD NOT OPEN LOCAL FILE:'
        CALL BORT2(BORT_STR,LOCFIL)
        END
