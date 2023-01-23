C> @file
C> @brief ???
C> @author ??? @date ???

C> ??? ~description~
C>
C> @author ??? @date


	MODULE MODA_USRINT

C> ??? description of NVAL
	  INTEGER, ALLOCATABLE :: NVAL(:)
C> ??? description of INV
	  INTEGER, TARGET, ALLOCATABLE :: INV(:,:)
C> ??? description of NRFELM
	  INTEGER, ALLOCATABLE :: NRFELM(:,:)
C> ??? description of VAL
	  REAL*8, TARGET, ALLOCATABLE :: VAL(:,:)

	END MODULE
