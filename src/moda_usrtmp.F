C> @file
C> @brief Temporary space used when reading a subset into memory.
C> @author J. Woollen 1994-01-06

C> Tese arrays are used in subrouine rcstpl to store subset segments
C> being copied from a subset template created in subroutine
C> makestab into internal subset arrays inv and val during a subset
C> input  being done in subroutine rdtree.
C>
C> @author J. Woollen 1994-01-06 


	MODULE MODA_USRTMP

        PARAMETER ( MAXRCR = 100 )

C> INV array elements for new sections of a growing subset buffer.
        INTEGER, ALLOCATABLE :: IUTMP(:,:)
C> VAL array elements for new sections of a growing subset buffer.
        REAL*8, ALLOCATABLE :: VUTMP(:,:)

        END MODULE
