C> @file
C> @brief Declare and initialize MXBTM variable.

C> This module declares and initializes the MXBTM variable.
C>
C> <p>This variable is initialized to a default value which can
C> be overridden by a subsequent call to function isetprm() within
C> the application program.
C>
C> @author J. Ator
C> @date 2014-12-10

        MODULE MODV_MXBTM

C>        @var mxbtm
C>        Maximum number of bitmaps that can be stored internally
C>        for a data subset.

          INTEGER :: MXBTM = 5

        END MODULE
