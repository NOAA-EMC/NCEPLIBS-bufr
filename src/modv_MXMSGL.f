C> @file
C> @brief Declare and initialize MXMSGL variable.

C> This module declares and initializes the MXMSGL variable.
C>
C> <p>This variable is initialized to a default value which can
C> be overridden by a subsequent call to function isetprm() within
C> the application program.
C>
C> @author J. Ator
C> @date 2014-12-10

        MODULE MODV_MXMSGL

C>        @var mxmsgl
C>        Maximum length (in bytes) of a BUFR message that can be
C>        read or written by the BUFRLIB software.

C>        @var mxmsgld4
C>        The value of mxmsgl divided by 4.

          INTEGER :: MXMSGL = 600000
          INTEGER :: MXMSGLD4

        END MODULE
