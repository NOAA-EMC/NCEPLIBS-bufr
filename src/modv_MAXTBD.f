C> @file
C> @brief Declare and initialize MAXTBD variable.

C> This module declares and initializes the MAXTBD variable.
C>
C> <p>This variable is initialized to a default value which can
C> be overridden by a subsequent call to function isetprm() within
C> the application program.
C>
C> @author J. Ator
C> @date 2014-12-10

        MODULE MODV_MAXTBD

C>        @var maxtbd
C>        Maximum number of entries in the internal BUFR Table D for
C>        each BUFR file that is connected to the BUFRLIB software.

          INTEGER :: MAXTBD = 500

        END MODULE
