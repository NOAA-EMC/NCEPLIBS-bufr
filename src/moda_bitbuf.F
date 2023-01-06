C> @file
C> @brief Declare arrays for internal storage of BUFR messages.
C> @author J. Ator @date 2014-12-10

C> This module contains array and variable declarations used to
C> store BUFR messages internally for multiple I/O streams.
C>
C> @author J. Ator
C> @date 2014-12-10

	MODULE MODA_BITBUF

C>        Maximum length of an output BUFR message.
	  INTEGER :: MAXBYT
C>        ???
	  INTEGER :: IBIT
C>        ???
	  INTEGER, ALLOCATABLE :: IBAY(:)
C>        Length (in bytes) of current BUFR message for each internal
C>        I/O stream.
	  INTEGER, ALLOCATABLE :: MBYT(:)
C>        Current BUFR message for each internal I/O stream.
	  INTEGER, ALLOCATABLE :: MBAY(:,:)

	END MODULE
