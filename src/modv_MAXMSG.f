C> @file
C> @brief Declare and initialize MAXMSG variable.

C> This module declares and initializes the MAXMSG variable.
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

	MODULE MODV_MAXMSG

C>        @var maxmsg
C>        Maximum number of BUFR messages that can be stored
C>        within internal memory.

	  INTEGER :: MAXMSG = 200000

	END MODULE
