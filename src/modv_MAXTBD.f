C> @file
C> @brief Declare and initialize MAXTBD variable.

C> This module declares and initializes the MAXTBD variable.
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

	MODULE MODV_MAXTBD

C>        @var maxtbd
C>        Maximum number of entries in the internal BUFR Table D for
C>        each BUFR file that is connected to the BUFRLIB software.

	  INTEGER :: MAXTBD = 500

	END MODULE
