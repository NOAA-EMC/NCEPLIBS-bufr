C> @file
C> @brief Declare and initialize MXBTMSE variable.

C> This module declares and initializes the MXBTMSE variable.
C>
C> <p>This variable is initialized to a default value which can
C> be overridden by a subsequent call to function isetprm() within
C> the application program.
C>
C> @author J. Ator
C> @date 2014-12-10

        MODULE MODV_MXBTMSE

C>        @var mxbtmse
C>        Maximum number of "set" entries (set to a value of 0)
C>        within a bitmap.

          INTEGER :: MXBTMSE = 500

        END MODULE
