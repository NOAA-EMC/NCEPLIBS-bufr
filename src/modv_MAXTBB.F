C> @file
C> @brief Declare and initialize MAXTBB variable.

C> This module declares and initializes the MAXTBB variable.
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

	MODULE MODV_MAXTBB

C>        @var maxtbb
C>        Maximum number of entries in the internal BUFR Table B for
C>        each BUFR file that is connected to the BUFRLIB software.

#ifdef STATIC_ALLOCATION
	  INTEGER, PARAMETER :: MAXTBB = 500
#else
	  INTEGER :: MAXTBB = 500
#endif

	END MODULE
