C> @file
C> @brief Declare arrays for internal storage of tracking which logical
C> units share DX BUFR tables.
C> @author J. Woollen @date 1994-01-06

C> This module contains a declaration for an array used by subroutine
C> makestab() to keep track of which logical units share DX BUFR
C> table information.
C>
C> @author J. Woollen @date 1994-01-06

	MODULE MODA_LUSHR

C>    Tracking index for each I/O internal stream index.  Set to a value
C>    of zero if the corresponding logical unit does not share DX BUFR
C>    table information with any other logical unit.  Otherwise set to a
C>    non-zero value within subroutine makestab().
	  INTEGER, ALLOCATABLE :: LUS(:)

	END MODULE
