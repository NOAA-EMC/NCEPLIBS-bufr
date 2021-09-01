C> @file
C> @brief Declare and initialize MAXJL variable.

C> This module declares and initializes the MAXJL variable.
C>
C> <p>This variable is initialized to a default value which can
C> be overridden by a subsequent call to function isetprm() within
C> the application program.
C>
C> @author J. Ator
C> @date 2014-12-10

	MODULE MODV_MAXJL

C>        @var maxjl
C>        Maximum number of entries in the internal jump/link table.

	  INTEGER :: MAXJL = 96000

	END MODULE
