C> @file
C> @brief Declare and initialize MXCDV variable.

C> This module declares and initializes the MXCDV variable.
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

	MODULE MODV_MXCDV

C>        @var mxcdv
C>        Maximum number of data values that can be written into
C>        a data subset of a compressed BUFR message by the
C>        BUFRLIB software.

	  INTEGER :: MXCDV = 3000

	END MODULE
