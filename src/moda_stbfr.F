C> @file
C> @brief Declare arrays for internal storage of file and message status
C> indicators.
C>
C> @author J. Woollen @date 1994-01-06

C> This module contains array declarations used to store file and message
C> status indicators for all logical units that have been connected to the
C> library via previous calls to subroutine openbf().
C>
C> @author J. Woollen @date 1994-01-06

	MODULE MODA_STBFR

C>    File status indicators.  Every connected I/O stream index has a
C>    non-zero value in this array:
C>    - if the value is positive, then the logical unit number of this
C>      same value is connected for output (i.e. writing/encoding) BUFR
C>    - if the value is negative, then the logical unit number of the
C>      absolute value of this same value is connected for input
C>      (i.e. reading/decoding) BUFR
	  INTEGER, ALLOCATABLE :: IOLUN(:)
C>    Message status indicator corresponding to iolun, denoting whether
C>    a BUFR message is currently open within the internal arrays for
C>    the corresponding logical unit:
C>    - 0 no
C>    - 1 yes
	  INTEGER, ALLOCATABLE :: IOMSG(:)

	END MODULE
