	SUBROUTINE MTFNAM ( IMT, IMTV, IOGCE, IMTVL, TBLTYP,
     .                      STDFIL, LOCFIL )

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    MTFNAM
C   PRGMMR: ATOR            ORG: NCEP       DATE: 2017-10-16
C
C ABSTRACT:  BASED ON THE INPUT ARGUMENTS, THIS SUBROUTINE DETERMINES
C   THE NAMES OF THE CORRESPONDING STANDARD AND LOCAL MASTER TABLE
C   FILES.  IT THEN CONFIRMS THE EXISTENCE OF THESE FILES ON THE
C   FILESYSTEM, USING ADDITIONAL INFORMATION OBTAINED FROM THE MOST
C   RECENT CALL TO SUBROUTINE MTINFO, OR ELSE AS DEFINED WITHIN
C   SUBROUTINE BFRINI IF SUBROUTINE MTINFO WAS NEVER CALLED.
C
C PROGRAM HISTORY LOG:
C 2017-10-16  J. ATOR    -- ORIGINAL AUTHOR
C
C USAGE:    CALL MTFNAM ( IMT, IMTV, IOGCE, IMTVL, TBLTYP,
C                         STDFIL, LOCFIL )
C   INPUT ARGUMENT LIST:
C     IMT      - INTEGER: MASTER TABLE NUMBER
C     IMTV     - INTEGER: MASTER TABLE VERSION NUMBER
C     IOGCE    - INTEGER: ORIGINATING CENTER
C     IMTVL    - INTEGER: LOCAL TABLE VERSION NUMBER
C     TBLTYP   - CHARACTER*(*): TABLE TYPE:
C                  'TableB'   = Table B
C                  'TableD'   = Table D
C                  'CodeFlag' = Code and Flag Tables 
C
C   OUTPUT ARGUMENT LIST:
C     STDFIL   - CHARACTER*(*): STANDARD MASTER TABLE PATH/FILENAME
C     LOCFIL   - CHARACTER*(*): LOCAL MASTER TABLE PATH/FILENAME
C
C REMARKS:
C    THIS ROUTINE CALLS:        BORT2    ERRWRT   ISIZE   STRSUC
C    THIS ROUTINE IS CALLED BY: IREADMT
C                               Normally not called by any application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

	INCLUDE 'bufrlib.prm'

	COMMON /QUIET/  IPRT
	COMMON /MSTINF/ LUN1, LUN2, LMTD, MTDIR

	CHARACTER*(*)	STDFIL, LOCFIL, TBLTYP

	CHARACTER*16	TBLTYP2
	CHARACTER*20	FMTF
	CHARACTER*100	MTDIR
	CHARACTER*128	BORT_STR
	LOGICAL		FOUND

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

	CALL STRSUC ( TBLTYP, TBLTYP2, LTBT )

C*	Determine the standard master table path/filename.

	IF ( ( IMT .EQ. 0 ) .AND. ( IMTV .LE. 13 ) ) THEN

C*	  For master table 0, version 13 is a superset of all earlier
C*	  versions.

	  STDFIL = MTDIR(1:LMTD) // '/bufrtab.' // TBLTYP2(1:LTBT) //
     .		   '_STD_0_13'
	ELSE
	  WRITE ( FMTF, '(A,I1,A,I1,A)' )
     .	     '(4A,I', ISIZE(IMT), ',A,I', ISIZE(IMTV), ')'
	  WRITE ( STDFIL, FMTF ) MTDIR(1:LMTD), '/bufrtab.',
     .	     TBLTYP2(1:LTBT), '_STD_', IMT, '_', IMTV
	ENDIF
	IF ( IPRT .GE. 2 ) THEN
	  CALL ERRWRT('Standard ' // TBLTYP2(1:LTBT) // ':')
	  CALL ERRWRT(STDFIL)
	ENDIF
	INQUIRE ( FILE = STDFIL, EXIST = FOUND )
	IF ( .NOT. FOUND ) GOTO 900

C*	Now determine the local master table path/filename.

C*	Use the local table corresponding to the originating center
C*	and local table version number, if such a table exists.
C*	Otherwise use the local table from NCEP.

	WRITE ( FMTF, '(A,I1,A,I1,A,I1,A)' )
     .	   '(4A,I', ISIZE(IMT), ',A,I', ISIZE(IOGCE),
     .	   ',A,I',  ISIZE(IMTVL), ')'
	WRITE ( LOCFIL, FMTF ) MTDIR(1:LMTD), '/bufrtab.',
     .	     TBLTYP2(1:LTBT), '_LOC_', IMT, '_', IOGCE, '_', IMTVL
	IF ( IPRT .GE. 2 ) THEN
	  CALL ERRWRT('Local ' // TBLTYP2(1:LTBT) // ':')
	  CALL ERRWRT(LOCFIL)
	ENDIF
	INQUIRE ( FILE = LOCFIL, EXIST = FOUND )
	IF ( .NOT. FOUND ) THEN

C*	  Use the local table from NCEP.

	  LOCFIL = MTDIR(1:LMTD) // '/bufrtab.' // TBLTYP2(1:LTBT) //
     .		   '_LOC_0_7_1'
	  IF ( IPRT .GE. 2 ) THEN
	    CALL ERRWRT('Local ' // TBLTYP2(1:LTBT) //
     .			'not found, so using:')
	    CALL ERRWRT(LOCFIL)
	  ENDIF
	  INQUIRE ( FILE = LOCFIL, EXIST = FOUND )
	  IF ( .NOT. FOUND ) GOTO 901
	ENDIF

	RETURN
900	BORT_STR = 'BUFRLIB: MTFNAM - COULD NOT FIND STANDARD FILE:'
	CALL BORT2(BORT_STR,STDFIL)
901	BORT_STR = 'BUFRLIB: MTFNAM - COULD NOT FIND LOCAL FILE:'
	CALL BORT2(BORT_STR,LOCFIL)
	END
