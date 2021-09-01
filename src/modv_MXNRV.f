C> @file
C> @brief Declare and initialize MXNRV variable.

C> This module declares and initializes the MXNRV variable.
C>
C> <p>This variable is initialized to a default value which can
C> be overridden by a subsequent call to function isetprm() within
C> the application program.
C>
C> @author J. Ator
C> @date 2014-12-10

	MODULE MODV_MXNRV

C>        @var mxnrv
C>        Maximum number of entries in the internal jump/link table
C>        that can contain new reference values.

	  INTEGER :: MXNRV = 15

	END MODULE
