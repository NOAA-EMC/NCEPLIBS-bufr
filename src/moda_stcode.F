C> @file
C> @brief Declare arrays for internal storage of abnormal status codes.
C> @author J. Ator @date 2010-05-11

C> This module contains an array declaration used to store a status
C> code for each internal I/O stream index if an error or other
C> abnormal result occurs while processing a BUFR message within
C> the associated logical unit.  The status code can be retrieved at
C> any time by the application program via a call to
C> subroutine igetsc().
C>
C> @author J. Ator @date 2010-05-11

	MODULE MODA_STCODE

C>    Abnormal status codes.
C>    - 0 all is normal; no error occurred
	  INTEGER, ALLOCATABLE :: ISCODES(:)

	END MODULE
