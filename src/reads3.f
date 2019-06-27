	SUBROUTINE READS3 ( LUN )

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    READS3
C   PRGMMR: ATOR             ORG: NP12       DATE: 2009-03-23
C
C ABSTRACT: THIS SUBROUTINE READS THE SECTION 3 DESCRIPTORS FROM THE
C   BUFR MESSAGE IN MBAY(1,LUN).  IT THEN USES THE BUFR MASTER TABLES
C   TO GENERATE THE NECESSARY INFORMATION FOR THESE DESCRIPTORS WITHIN
C   THE INTERNAL BUFR TABLE ARRAYS.
C
C PROGRAM HISTORY LOG:
C 2009-03-23  J. ATOR    -- ORIGINAL AUTHOR
C 2014-12-10  J. ATOR    -- USE MODULES INSTEAD OF COMMON BLOCKS
C 2017-10-13  J. ATOR    -- REMOVE FUNCTIONALITY TO CHECK WHETHER NEW
C                           MASTER TABLES NEED TO BE READ (THIS
C                           FUNCTIONALITY IS NOW PART OF FUNCTION
C                           IREADMT)

C
C USAGE:    CALL READS3 ( LUN )
C   INPUT ARGUMENT LIST:
C     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
C
C REMARKS:
C    THIS ROUTINE CALLS:        ADN30    BORT     DXINIT   ERRWRT
C                               IFXY     IGETNTBI IGETTDI  IREADMT
C                               MAKESTAB STNTBIA  STSEQ    UPDS3
C    THIS ROUTINE IS CALLED BY: READERME READMG
C                               Normally not called by any application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

	USE MODA_SC3BFR
	USE MODA_BITBUF

	INCLUDE 'bufrlib.prm'

	COMMON /QUIET/  IPRT
	COMMON /DSCACH/	NCNEM,CNEM(MXCNEM),NDC(MXCNEM),
     .                  IDCACH(MXCNEM,MAXNC)

	DIMENSION	IDS3(MAXNC)
	CHARACTER*6	CDS3(MAXNC),NUMB,ADN30

	CHARACTER*8	CNEM
	CHARACTER*55	CSEQ

	CHARACTER*128	ERRSTR

	LOGICAL		INCACH

	SAVE	IREPCT

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C*	Check whether the appropriate BUFR master table information has
C*	already been read into internal memory for this message.

	IF ( IREADMT ( LUN ) .EQ. 1 ) THEN

C*	  NO (i.e. we just had to read in new master table information
C*	  for this message), so reset some corresponding values in
C*	  other parts of the library.

	  CALL DXINIT ( LUN, 0 )
	  ITMP = IGETTDI ( 0 )
	  IREPCT = 0
	  NCNEM = 0
	ENDIF

C*	Unpack the list of Section 3 descriptors from the message.

	CALL UPDS3 ( MBAY(1,LUN), MAXNC, CDS3, NCDS3 )
	DO II = 1, NCDS3
	  IDS3(II) = IFXY( CDS3(II) )
	ENDDO

C*	Is the list of Section 3 descriptors already in the cache?

C*	The cache is a performance-enhancing device which saves
C*	time when the same descriptor sequences are encountered
C*	over and over within the calling program.  Time is saved
C*	because the below calls to subroutines STSEQ and MAKESTAB
C*	are bypassed whenever a list is already in the cache.

	INCACH = .FALSE.
	IF ( NCNEM .GT. 0 ) THEN
	  II = 1
	  DO WHILE ( (.NOT.INCACH) .AND. (II.LE.NCNEM) )
	    IF ( NCDS3 .EQ. NDC(II) ) THEN
	      JJ = 1
	      INCACH = .TRUE.
	      DO WHILE ( (INCACH) .AND. (JJ.LE.NCDS3) )
		IF ( IDS3(JJ) .EQ. IDCACH(II,JJ) ) THEN
		  JJ = JJ + 1
		ELSE
		  INCACH = .FALSE.
		ENDIF
	      ENDDO
	      IF (INCACH) THEN

C*		The list is already in the cache, so store the
C*		corresponding Table A mnemonic into MODULE SC3BFR
C*		and return.

		IF ( IPRT .GE. 2 ) THEN
	CALL ERRWRT('+++++++++++++++++++++++++++++++++++++++++++++++++')
	ERRSTR = 'BUFRLIB: READS3 - RE-USED CACHE LIST FOR ' // CNEM(II)
	CALL ERRWRT(ERRSTR)
	CALL ERRWRT('+++++++++++++++++++++++++++++++++++++++++++++++++')
        CALL ERRWRT(' ')
		ENDIF
		TAMNEM(LUN) = CNEM(II)
		RETURN
	      ENDIF
	    ENDIF
	    II = II + 1
	  ENDDO
	ENDIF

C*	Get the next available index within the internal Table A.

	N = IGETNTBI ( LUN, 'A' )

C*	Generate a Table A mnemonic and sequence description.

	WRITE ( TAMNEM(LUN), '(A5,I3.3)') 'MSTTB', N
	CSEQ = 'TABLE A MNEMONIC ' // TAMNEM(LUN)

C*	Store the Table A mnemonic and sequence into the cache.

	NCNEM = NCNEM + 1
	IF ( NCNEM .GT. MXCNEM ) GOTO 900
	CNEM(NCNEM) = TAMNEM(LUN)
	NDC(NCNEM) = NCDS3
	DO JJ = 1, NCDS3
	  IDCACH(NCNEM,JJ) = IDS3(JJ)
	ENDDO
	IF ( IPRT .GE. 2 ) THEN
	CALL ERRWRT('+++++++++++++++++++++++++++++++++++++++++++++++++')
	ERRSTR = 'BUFRLIB: READS3 - STORED CACHE LIST FOR ' //
     .    CNEM(NCNEM)
	CALL ERRWRT(ERRSTR)
	CALL ERRWRT('+++++++++++++++++++++++++++++++++++++++++++++++++')
        CALL ERRWRT(' ')
	ENDIF

C*	Get an FXY value to use with this Table A mnemonic.
  
	IDN = IGETTDI ( LUN )
	NUMB = ADN30 ( IDN, 6 )

C*	Store all of the information for this mnemonic within the
C*	internal Table A.

	CALL STNTBIA ( N, LUN, NUMB, TAMNEM(LUN), CSEQ )

C*	Store all of the information for this sequence within the
C*	internal Tables B and D.

	CALL STSEQ ( LUN, IREPCT, IDN, TAMNEM(LUN), CSEQ, IDS3, NCDS3 )

C*	Update the jump/link table.

	CALL MAKESTAB

	RETURN
900	CALL BORT('BUFRLIB: READS3 - MXCNEM OVERFLOW')
	END
