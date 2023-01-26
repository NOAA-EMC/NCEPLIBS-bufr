C> @file
C> @brief Declare arrays for internal storage of BUFR message types.
C>
C> @author J. Woollen @date 1994-01-06

C> This module contains an array declaration used to store, for each
C> I/O stream index from which a BUFR message is currently being read
C> as input, the type of the BUFR message.
C>
C> @author J. Woollen @date 1994-01-06

	MODULE MODA_UNPTYP

C>      BUFR message types:
C>      - 0 message contains data subset byte counters and other
C>      non-standard enhancements
C>      - 1 message is fully standard and contains no
C>      non-standard enhancements
C>      - 2 message is compressed
	  INTEGER, ALLOCATABLE :: MSGUNP(:)

	END MODULE
