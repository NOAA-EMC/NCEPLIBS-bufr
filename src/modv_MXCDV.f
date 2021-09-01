C> @file
C> @brief Declare and initialize MXCDV variable.

C> This module declares and initializes the MXCDV variable.
C>
C> <p>This variable is initialized to a default value which can
C> be overridden by a subsequent call to function isetprm() within
C> the application program.
C>
C> @author J. Ator
C> @date 2014-12-10

	MODULE MODV_MXCDV

C>        @var mxcdv
C>        Maximum number of data values that can be written into
C>        a data subset of a compressed BUFR message by the
C>        BUFRLIB software.

	  INTEGER :: MXCDV = 3000

	END MODULE
