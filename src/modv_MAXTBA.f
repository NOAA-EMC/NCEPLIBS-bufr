C> @file
C> @brief Declare and initialize MAXTBA variable.

C> This module declares and initializes the MAXTBA variable.
C>
C> <p>This variable is initialized to a default value which can
C> be overridden by a subsequent call to function isetprm() within
C> the application program.
C>
C> @author J. Ator
C> @date 2014-12-10

	MODULE MODV_MAXTBA

C>        @var maxtba
C>        Maximum number of entries in the internal BUFR Table A for
C>        each BUFR file that is connected to the BUFRLIB software.

	  INTEGER :: MAXTBA = 150

	END MODULE
