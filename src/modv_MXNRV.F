C> @file
C> @brief Declare and initialize MXNRV variable.

C> This module declares and initializes the MXNRV variable.
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

	MODULE MODV_MXNRV

C>        @var mxnrv
C>        Maximum number of entries in the internal jump/link table
C>        that can contain new reference values.

#ifdef STATIC_ALLOCATION
	  INTEGER, PARAMETER :: MXNRV = 15
#else
	  INTEGER :: MXNRV = 15
#endif

	END MODULE
