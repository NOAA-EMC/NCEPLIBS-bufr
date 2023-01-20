C> @file
C> @brief ???
C> @author ??? @date ???

C> ??? ~description~
C>
C> @author ??? @date ???

	MODULE MODA_COMPRX

C>  ??? description of NROW
	  INTEGER :: NROW
C>  ??? description of LUNC
	  INTEGER :: LUNC
C>  ??? description of KBYT
	  INTEGER :: KBYT
C>  ??? description of FLUSH
	  LOGICAL :: FLUSH
C>  ??? description of WRIT1
	  LOGICAL :: WRIT1
C>  ??? description of KMIN
	  INTEGER(8), ALLOCATABLE :: KMIN(:)
C>  ??? description of KMAX
	  INTEGER(8), ALLOCATABLE :: KMAX(:) 
C>  ??? description of KMIS
	  LOGICAL   , ALLOCATABLE :: KMIS(:)
C>  ??? description of KBIT
	  INTEGER   , ALLOCATABLE :: KBIT(:)
C>  ??? description of ITYP
	  INTEGER   , ALLOCATABLE :: ITYP(:)
C>  ??? description of IWID
	  INTEGER   , ALLOCATABLE :: IWID(:)
C>  ??? description of CSTR
	  CHARACTER*(:), ALLOCATABLE :: CSTR(:)

	END MODULE
