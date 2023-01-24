C> @file
C> @brief Determine filenames and check for the existence of
C> corresponding standard and local master table files on the
C> filesystem.
C> @author Ator @date 2017-10-16
	
C> Based on the input arguments, this subroutine determines
c> the names of the corresponding standard and local master table
c> files. It then confirms the existence of these files on the
c> filesystem, using additional information obtained from the most
c> recent call to subroutine mtinfo(), or else as defined within
c> subroutine bfrini() if subroutine mtinfo() was never called.
C>
C> @param[in] IMT - integer: master table number.
C> @param[in] IMTV - integer: master table version number.
C> @param[in] IOGCE - integer: originating center.
C> @param[in] IMTVL - integer: local table version number.
C> @param[in] TBLTYP - character*(*): table type:.
C> - 'TableB' Table B
C> - 'TableD' Table D
C> - 'CodeFlag' Code and Flag Tables 
C> @param[out] STDFIL - character*(*): standard master table path/filename.
C> @param[out] LOCFIL - character*(*): local master table path/filename.
C>
C> @author Ator @date 2017-10-16
	SUBROUTINE MTFNAM ( IMT, IMTV, IOGCE, IMTVL, TBLTYP,
     .                      STDFIL, LOCFIL )

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
