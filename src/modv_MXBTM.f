C> @file
C> @brief Declare and initialize MXBTM variable.

C> This module declares and initializes the MXBTM variable.
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

	MODULE MODV_MXBTM

C>        @var mxbtm
C>        Maximum number of bitmaps that can be stored internally
C>        for a data subset.

	  INTEGER :: MXBTM = 5

	END MODULE
