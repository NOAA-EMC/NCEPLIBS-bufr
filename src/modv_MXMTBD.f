C> @file
C> @brief Declare and initialize MXMTBD variable.

C> This module declares and initializes the MXMTBD variable.
C>
C> <p>This variable is initialized to a default value which can
C> be overridden by a subsequent call to function isetprm() within
C> the application program.
C>
C> @author J. Ator
C> @date 2014-12-10

	MODULE MODV_MXMTBD

C>        @var mxmtbd
C>        Maximum number of entries in a master BUFR Table D.

	  INTEGER :: MXMTBD = 1000

	END MODULE
