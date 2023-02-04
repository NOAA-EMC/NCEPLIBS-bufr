C> @file
C> @brief Declare arrays for internal storage of long character strings.
C>
C> @author J. Woollen @date 2009-03-23

C> This module contains array and variable declarations needed to store
C> information about long character strings (greater than 8 bytes) when
C> reading them from input data subsets in compressed BUFR messages via
C> subroutines rdcmps() and readlc().
C>
C> @author J. Woollen @date 2009-03-23

        MODULE MODA_RLCCMN

C>    Number of long character strings in data subset.
          INTEGER :: NRST
C>    Lengths (in bytes) of long character strings.
          INTEGER, ALLOCATABLE :: IRNCH(:)
C>    Pointers in data subset to first bits of long character strings.
          INTEGER, ALLOCATABLE :: IRBIT(:)
C>    Table B mnemonics associated with long character strings.
          CHARACTER*10, ALLOCATABLE :: CRTAG(:)

        END MODULE
