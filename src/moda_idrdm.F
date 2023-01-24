C> @file
C> @brief Declare arrays for internal storage of DX BUFR tables messages.
C>
C> @author J. Ator @date 2009-03-23

C> This module contains a declaration for an array used by subroutine readerme()
C> to read in a new DX dictionary table as a consecutive set of one or more
C> DX BUFR tables messages.
C>
C> @author J. Ator @date 2009-03-23

	MODULE MODA_IDRDM

C>    DX BUFR tables message count for each I/O internal stream index.  Set to
C>    a value of zero unless a new DX dictionary table is in the process of being
C>    read in by subroutine readerme() for the associated logical unit, and in
C>    which case it keeps track of how many such messages have been read in so far.
	  INTEGER, ALLOCATABLE :: IDRDM(:)

	END MODULE
