C> @file
C> @brief Declare arrays for internal storage of pointers to data values.
C>
C> @author J. Woollen @date 1994-01-06

C> This module contains array declarations for internal storage of
C> pointers to BUFR data subset values.  They are used when unpacking an
C> input data subset via subroutines rcstpl() and rdtree().
C>
C> @author J. Woollen @date 1994-01-06

        MODULE MODA_USRBIT

C>        Length (in bits) of each packed value in mbit.
          INTEGER, ALLOCATABLE :: NBIT(:)
C>        Pointer in data subset to first bit of each packed data value.
          INTEGER, ALLOCATABLE :: MBIT(:)

        END MODULE
