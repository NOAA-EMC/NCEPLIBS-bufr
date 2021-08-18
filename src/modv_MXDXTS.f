C> @file
C> @brief Declare and initialize MXDXTS variable.

C> This module declares and initializes the MXDXTS variable.
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

	MODULE MODV_MXDXTS

C>        @var mxdxts
C>        Maximum number of dictionary tables that can be stored
C>        for use with BUFR messages in internal memory.

	  INTEGER :: MXDXTS = 200

	END MODULE
