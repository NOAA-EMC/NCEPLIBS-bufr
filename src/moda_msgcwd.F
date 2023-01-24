C> @file
C> @brief Declare arrays for internal storage of BUFR message information.
C>
C> @author J. Woollen @date 1994-01-06

C> This module contains declarations for arrays used to store information
C> about the current BUFR message that is in the process of being read from
C> or written to the logical unit associated with each internal I/O stream
C> index.
C>
C> @author J. Woollen @date 1994-01-06

	MODULE MODA_MSGCWD

C>    Current message pointer within logical unit.
	  INTEGER, ALLOCATABLE :: NMSG(:)
C>    Current subset pointer within message.
	  INTEGER, ALLOCATABLE :: NSUB(:)
C>    Total number of data subsets in message.
	  INTEGER, ALLOCATABLE :: MSUB(:)
C>    Table A mnemonic for type of BUFR message.
	  INTEGER, ALLOCATABLE :: INODE(:)
C>    Section 1 date-time of message.
	  INTEGER, ALLOCATABLE :: IDATE(:)

	END MODULE
