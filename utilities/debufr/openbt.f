	SUBROUTINE OPENBT ( lundx, mtyp )

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    OPENBT
C   PRGMMR: J. ATOR          ORG: NP12        DATE: 2012-12-07
C
C ABSTRACT: Given a BUFR message type, this subroutine opens the
C   appropriate DX dictionary table in the specified table directory
C   as Fortran logical unit "lundx".  This subroutine overrides the
C   default subroutine of the same name in the NCEP BUFRLIB.
C
C PROGRAM HISTORY LOG:
C 2012-12-07  J. ATOR -- ORIGINAL AUTHOR
C
C USAGE:    CALL OPENBT ( lundx, mtyp )
C   INPUT ARGUMENT LIST:
C     mtyp     - integer: message type of input BUFR file
C
C   OUTPUT ARGUMENT LIST:
C     lundx    - integer: unit number of BUFR mnemonic table
C		   0 = unable to open table
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  Portable to all platforms
C
C$$$

	USE Share_Table_Info

	CHARACTER*11	bftab

	CHARACTER*240	bftabfil

	LOGICAL		exists

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

	WRITE ( bftab, '("bufrtab.",i3.3)' ) mtyp
	bftabfil = ctbldir(1:ltbd) // '/' // bftab

	INQUIRE ( FILE = bftabfil, EXIST = exists )
	IF ( exists ) THEN
	    lundx = ludx
	    CLOSE ( lundx )
	    OPEN ( UNIT = lundx, FILE = bftabfil )
	ELSE
	    lundx = 0
	END IF

	RETURN
	END
