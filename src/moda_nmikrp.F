C> @file
C> @brief Declare arrays for internal storage of Table D information.
C>
C> @author J. Woollen @date 1994-01-06

C> This module contains declarations for arrays used by various
C> subroutines to hold information about Table D sequences.  These
C> values are set via an internal call to subroutine nemtbd().
C>
C> @author J. Woollen @date 1994-01-06

        MODULE MODA_NMIKRP

C>    Child mnemonics within Table D sequences.
          CHARACTER*8, ALLOCATABLE :: NEM(:,:)
C>    Replication indicators corresponding to nem:
C>    - 5, if corresponding nem is a Table D mnemonic using 1-bit delayed replication
C>    - 4, if corresponding nem is a Table D mnemonic using 8-bit delayed (stack) replication
C>    - 3, if corresponding nem is a Table D mnemonic using 8-bit delayed replication
C>    - 2, if corresponding nem is a Table D mnemonic using 16-bit delayed replication
C>    - 1, if corresponding nem is a Table D mnemonic using regular (non-delayed) replication
C>    - 0, otherwise
          INTEGER, ALLOCATABLE :: IRP(:,:)
C>    Replication counts corresponding to nem:
C>    - Number of replications, if corresponding nem is a Table D mnemonic using regular
C>      (non-delayed) replication
C>    - 0, otherwise
          INTEGER, ALLOCATABLE :: KRP(:,:)

        END MODULE
