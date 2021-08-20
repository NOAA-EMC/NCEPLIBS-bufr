C> @file
C> @brief Declare and initialize MAXTBA variable.

C> This module declares and initializes the MAXTBA variable.
C>
C> <p>For dynamic allocation builds, this variable is initialized
C> to a default value which can be overridden by a subsequent call
C> to function isetprm() within the application program.
C> For static allocation builds, this variable is set as a
C> parameter at compile time and cannot be changed within the
C> application program.
C>
C> @author J. Ator
C> @date 2014-12-10

	MODULE MODV_MAXTBA

C>        @var maxtba
C>        Maximum number of entries in the internal BUFR Table A for
C>        each BUFR file that is connected to the BUFRLIB software.

	  INTEGER :: MAXTBA = 150

	END MODULE
