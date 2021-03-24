C> @file
C> @brief Declare and initialize MXBTMSE variable.

C> This module declares and initializes the MXBTMSE variable.
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

	MODULE MODV_MXBTMSE

C>        @var mxbtmse
C>        Maximum number of "set" entries (set to a value of 0)
C>        within a bitmap.

#ifdef STATIC_ALLOCATION
	  INTEGER, PARAMETER :: MXBTMSE = 500
#else
	  INTEGER :: MXBTMSE = 500 
#endif

	END MODULE
