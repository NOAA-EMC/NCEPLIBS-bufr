C> @file
C> @brief Declare arrays and variables for internal storage of file rewind values.
C> @author J. Ator @date 2014-12-10

C> This module contains arrays and variables needed to store the current position within
C> a BUFR file.  This allows a rewind back to the first message within the file, while
C> retaining the capability to return to the original file position at a later point within
C> the application program.
C>
C> The current position can be stored for only one BUFR file at a time.
C>
C> @author J. Ator @date 2014-12-10

	MODULE MODA_BUFRSR

C>        Internal I/O stream index of BUFR file.
	  INTEGER :: JUNN
C>        File status indicator of BUFR file.
	  INTEGER :: JILL
C>        Message status indicator of BUFR file.
	  INTEGER :: JIMM
C>        Bit pointer within BUFR message.
	  INTEGER :: JBIT
C>        Length (in bytes) of BUFR message.
	  INTEGER :: JBYT
C>        Sequential number of BUFR message, counting from the beginning of the file.
	  INTEGER :: JMSG
C>        Sequential number of BUFR data subset, counting from the beginning of the
C>        current BUFR message.
	  INTEGER :: JSUB
C>        Bit-wise (integer) representation of FXY value associated with Table A
C>        mnemonic for BUFR message.
	  INTEGER :: KSUB
C>        Positional index of Table A mnemonic within internal Table A.
	  INTEGER :: JNOD
C>        Section 1 date-time of BUFR message.
	  INTEGER :: JDAT
C>        ???
	  INTEGER, ALLOCATABLE :: JSR(:)
C>        BUFR message.
	  INTEGER, ALLOCATABLE :: JBAY(:)

	END MODULE
