C> @file
C> @brief Declare and initialize MXRST variable.

C> This module declares and initializes the MXRST variable.
C>
C> <p>This variable is initialized to a default value which can
C> be overridden by a subsequent call to function isetprm() within
C> the application program.
C>
C> @author J. Ator
C> @date 2014-12-10

	MODULE MODV_MXRST

C>        @var mxrst
C>        Maximum number of "long" character strings (greater than 8
C>        bytes) that can be read from a data subset of a compressed
C>        BUFR message.

	  INTEGER :: MXRST = 50

	END MODULE
