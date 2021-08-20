C> @file
C> @brief Declare and initialize MXRST variable.

C> This module declares and initializes the MXRST variable.
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

	MODULE MODV_MXRST

C>        @var mxrst
C>        Maximum number of "long" character strings (greater than 8
C>        bytes) that can be read from a data subset of a compressed
C>        BUFR message.

	  INTEGER :: MXRST = 50

	END MODULE
