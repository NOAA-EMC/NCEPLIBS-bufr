C> @file
C> @brief Declare arrays and variables for internal storage of
C> compressed BUFR data subsets.
C> @author J. Woollen @date 2002-05-14

C> This module contains arrays and variable declarations for the
C> storage of data values needed when writing compressed data subsets
C> to a BUFR message for output (i.e. writing/encoding BUFR).
C>
C> This module can only be used to store the information for one
C> compressed BUFR message at any given time.
C>
C> @author J. Woollen @date 2002-05-14

	MODULE MODA_COMPRS

C>    Number of data subsets in message.
	  INTEGER                    :: NCOL
C>    Increment used when compressing non-character data values.
	  INTEGER(8)                 :: INCR
C>    Non-character data values for all data subsets in message.
	  INTEGER(8)   , ALLOCATABLE :: MATX(:,:) 
C>    Character data values for all data subsets in message.
	  CHARACTER*(:), ALLOCATABLE :: CATX(:,:)

	END MODULE
