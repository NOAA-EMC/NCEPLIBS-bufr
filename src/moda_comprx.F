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

	MODULE MODA_COMPRX

C>    Number of data values for each data subset in message.
	  INTEGER :: NROW
C>    I/O stream index into internal arrays for output file.
	  INTEGER :: LUNC
C>    Number of bytes required to store Sections 0, 1, 2, and 3 of
C>    message.
	  INTEGER :: KBYT
C>    Flush flag.  Set to .true. if a subroutine call was made to
C>    force the writing of the message to the corresponding output file,
C>    even if there may still be room in the message for additional
C>    data subsets.  Otherwise set to .false.
	  LOGICAL :: FLUSH
C>    Write-out flag.  Set to .true. if the message needs to be
C>    written to the corresponding output file.  Otherwise set to .false.
	  LOGICAL :: WRIT1
C>    Minimum of each data value across all data subsets in message.
	  INTEGER(8), ALLOCATABLE :: KMIN(:)
C>    Maximum of each data value across all data subsets in message.
	  INTEGER(8), ALLOCATABLE :: KMAX(:) 
C>    "Missing" values flag.  Set to .true. if at least one occurrence
C>    of this data value is "missing" within any data subset of the
C>    message. Otherwise set to .false.
	  LOGICAL   , ALLOCATABLE :: KMIS(:)
C>    Number of bits needed to hold the increments for this data value
C>    within each data subset of the message.
	  INTEGER   , ALLOCATABLE :: KBIT(:)
C>    Type of each data value.
C>    - 1 Delayed descriptor replication factor.
C>    - 2 Other non-character data.
C>    - 3 Character data.
	  INTEGER   , ALLOCATABLE :: ITYP(:)
C>    Bit width of underlying data descriptor as defined within Table B
C>    for each data value.
	  INTEGER   , ALLOCATABLE :: IWID(:)
C>    Character data value, if corresponding ITYP value is set to 3.
	  CHARACTER*(:), ALLOCATABLE :: CSTR(:)

	END MODULE
