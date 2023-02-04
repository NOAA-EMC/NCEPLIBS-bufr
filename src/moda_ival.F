C> @file
C> @brief Declare arrays for internal storage of data values.
C>
C> @author J. Woollen @date 1994-01-06

C> This module contains a declaration for an array used to pack or
C> unpack all of the values of a BUFR data subset.  It is used when
C> packing a data subset for output via subroutine wrtree(), or when
C> unpacking an input data subset via subroutine rdtree().
C>
C> @author J. Woollen @date 1994-01-06

        MODULE MODA_IVAL

C>    BUFR data subset values.
          INTEGER(8), ALLOCATABLE :: IVAL(:)

        END MODULE
