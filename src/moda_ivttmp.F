C> @file
C> @brief Declare arrays for internal storage of temporary arrays for
C> internal subset buffer expansions.
C>
C> @author J. Woollen @date 1994-01-06

C> These arrays provide working space in several subprograms (usrtpl() and
C> ufbcup()) which manipulate the contents of the internal subset buffers
C> where the contents of an evolving BUFR message are sccumulated and 
C> stored under user control prior to being written out.
C>
C> @author J. Woollen @date 1994-01-06

	MODULE MODA_IVTTMP

C>  TAG array elements for new sections of a growing subset buffer.
        CHARACTER*10, ALLOCATABLE :: TTMP(:)
C>  INV array elements for new sections of a growing subset buffer.
        INTEGER, ALLOCATABLE :: ITMP(:) 
C>  VAL array elements for new sections of a growing subset buffer.
        REAL*8, ALLOCATABLE :: VTMP(:)

	END MODULE
