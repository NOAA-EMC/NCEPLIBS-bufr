C> @file
C> @brief Declare and initialize MXDXTS variable.

C> This module declares and initializes the MXDXTS variable.
C>
C> <p>This variable is initialized to a default value which can
C> be overridden by a subsequent call to function isetprm() within
C> the application program.
C>
C> @author J. Ator
C> @date 2014-12-10

        MODULE MODV_MXDXTS

C>        @var mxdxts
C>        Maximum number of dictionary tables that can be stored
C>        for use with BUFR messages in internal memory.

          INTEGER :: MXDXTS = 200

        END MODULE
