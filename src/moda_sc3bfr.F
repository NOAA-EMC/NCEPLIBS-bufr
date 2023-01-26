C> @file
C> @brief Declare arrays for internal storage of Section 3 switches.
C>
C> @author J. Ator @date 2009-03-23

C> This module contains an array declaration used to store a switch for
C> each internal I/O stream index, indicating whether BUFR messages
C> read from the corresponding logical unit should be decoded according
C> to the contents of Section 3 and using master BUFR tables,
C> rather than using DX BUFR tables.
C>
C> The default value is "off", indicating that BUFR messages read from
C> the corresponding logical unit should be decoded using DX BUFR tables,
C> but it can be switched to a value of "on" if the corresponding logical
C> unit is connected to the library via a call to subroutine openbf()
C> with IO = 'SEC3', and in which case each BUFR message will be decoded
C> according to the contents of Section 3 and using master BUFR tables.
C>
C> @author J. Ator @date 2009-03-23

	MODULE MODA_SC3BFR

C>    Section 3 switch for each internal I/O stream index:
C>    - 0 BUFR messages read from corresponding logical unit will be
C>      decoded using DX BUFR tables (default)
C>    - 1 BUFR messages read from corresponding logical unit will be
C>      decoded using master BUFR tables
	  INTEGER, ALLOCATABLE :: ISC3(:)
C>    Table A mnemonic most recently read from each internal I/O stream
C>    index, if isc3 = 1 for that stream
	  CHARACTER*8, ALLOCATABLE :: TAMNEM(:)

	END MODULE
