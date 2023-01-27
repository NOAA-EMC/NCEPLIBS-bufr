C> @file
C> @brief Declare arrays for internal storage of data values and
C> associated metadata.
C>
C> @author J. Woollen @date 1994-01-06

C> This module contains declarations for arrays used to store data
C> values and associated metadata for the current BUFR data subset
C> in scope for each I/O stream index.
C>
C> This module is used by numerous subroutines throughout the library
C> which read or write BUFR data subsets.
C>
C> @author J. Woollen @date 1994-01-06

	MODULE MODA_USRINT

C>      Number of data values in BUFR data subset.
	  INTEGER, ALLOCATABLE :: NVAL(:)
C>      Inventory pointer which links each data value to its
C>      corresponding node in the internal jump/link table.
	  INTEGER, TARGET, ALLOCATABLE :: INV(:,:)
C>      Referenced data value, for data values which refer to a previous
C>      data value in the BUFR data subset via an internal bitmap.
	  INTEGER, ALLOCATABLE :: NRFELM(:,:)
C>      Data values.
	  REAL*8, TARGET, ALLOCATABLE :: VAL(:,:)

	END MODULE
