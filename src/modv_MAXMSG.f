C> @file
C> @brief Declare and initialize MAXMSG variable.

C> This module declares and initializes the MAXMSG variable.
C>
C> <p>This variable is initialized to a default value which can
C> be overridden by a subsequent call to function isetprm() within
C> the application program.
C>
C> @author J. Ator
C> @date 2014-12-10

        MODULE MODV_MAXMSG

C>        @var maxmsg
C>        Maximum number of BUFR messages that can be stored
C>        within internal memory.

          INTEGER :: MAXMSG = 200000

        END MODULE
