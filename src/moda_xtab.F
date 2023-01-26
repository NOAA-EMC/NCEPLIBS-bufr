C> @file
C> @brief Declare arrays for internal storage of tracking when DX BUFR
C> tables are changed.
C>
C> @author J. Woollen @date 2009-03-18

C> This module contains an array declaration used to track, for each
C> I/O stream index, whether the DX BUFR table associated with the
C> corresponding logical unit has changed during the life of the
C> application program.
C>
C> This information is stored and tracked within subroutine makestab(),
C> which checks each time it is called to see if the DX BUFR table has
C> changed for any I/O stream index since the last time it was called.
C>
C> @author J. Woollen @date 2009-03-18

	MODULE MODA_XTAB

C>      Tracking index for each internal I/O stream index.
C>      Set to .true. if the DX BUFR table for the corresponding
C>      logical unit has changed since the previous call to
C>      subroutine makestab(); set to .false. otherwise.
	  LOGICAL, ALLOCATABLE :: XTAB(:)

	END MODULE
