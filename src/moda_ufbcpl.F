C> @file
C> @brief Declare arrays for internal storage of logical units used to
C> copy BUFR data subsets.
C>
C> @author J. Woollen @date 2009-08-11

C> This module contains an array declaration used to store, for each
C> I/O stream index, the logical unit number corresponding to a separate
C> I/O stream index whenever BUFR data subsets are being copied
C> from the latter to the former via subroutine ufbcpy().  In such
C> cases, this stored logical unit number is later accessed within
C> subroutine wrtree() to enable the copying of long character
C> strings (greater than 8 bytes) between the two logical units.
C>
C> @author J. Woollen @date 2009-08-11

	MODULE MODA_UFBCPL

C>        Logical unit numbers used to copy long character strings
C>        between BUFR data subsets.
	  INTEGER, ALLOCATABLE :: LUNCPY(:)

	END MODULE
