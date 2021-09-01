C> @file
C> @brief Declare and initialize MAXTBB variable.

C> This module declares and initializes the MAXTBB variable.
C>
C> <p>This variable is initialized to a default value which can
C> be overridden by a subsequent call to function isetprm() within
C> the application program.
C>
C> @author J. Ator
C> @date 2014-12-10

	MODULE MODV_MAXTBB

C>        @var maxtbb
C>        Maximum number of entries in the internal BUFR Table B for
C>        each BUFR file that is connected to the BUFRLIB software.

	  INTEGER :: MAXTBB = 500

	END MODULE
