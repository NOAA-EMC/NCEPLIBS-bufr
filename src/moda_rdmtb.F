C> @file
C> @brief Declare arrays for internal storage of master Table B
C> and Table D entries.
C> @author J. Ator @date 2014-12-10

C> This module contains array and variable declarations used to
C> store master Table B and Table D entries within internal
C> memory.
C>
C> Data values within this module are stored by subroutine
C> ireadmt().
C>
C> @author J. Ator
C> @date 2014-12-10


	MODULE MODA_RDMTB

C>    Bit-wise representations of child descriptors of
C>    Table D sequences.
	  INTEGER, ALLOCATABLE :: IEFXYN(:,:)
C>    Descriptor codes for Table B elements.
	  CHARACTER*4, ALLOCATABLE :: CMDSCB(:)
C>    Descriptor codes for Table D sequences.
	  CHARACTER*4, ALLOCATABLE :: CMDSCD(:)
C>    Element names corresponding to iefxyn.
	  CHARACTER*120, ALLOCATABLE :: CEELEM(:,:)

	END MODULE
