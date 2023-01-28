C> @file
C> @brief This module contains temp space used for internal buffer expansions.
C> @author Woollen @date 1994-01-06

C> These arrays provide working space in several subprograms (usrtpl and
C> ufbcup) which manipulate the contents of the internal subset buffers
C> where the contents of an evolving BUFR message are sccumulated and 
C> stored under user control prior to being written out.
C>
C>  TTMP - space used to store TAG elementS for new sections of a growing subset buffer
C>  ITMP - space used to store INV elements for new sections of a growing subset buffer
C>  VTMP - space used to store VAL elements  for new sections of a growing subset buffer
C>
C> @author Woollen @date 1994-01-06

	MODULE MODA_IVTTMP

        CHARACTER*10, ALLOCATABLE :: TTMP(:)
        INTEGER, ALLOCATABLE :: ITMP(:) 
        REAL*8, ALLOCATABLE :: VTMP(:)

	END MODULE
