C> @file
C> @brief Declare and initialize MAXMEM variable.

C> This module declares and initializes the MAXMEM variable.
C>
C> <p>This variable is initialized to a default value which can
C> be overridden by a subsequent call to function isetprm() within
C> the application program.
C>
C> @author J. Ator
C> @date 2014-12-10

	MODULE MODV_MAXMEM

C>        @var maxmem
C>        Maximum number of bytes that can be used to store BUFR
C>        messages within internal memory.

	  INTEGER :: MAXMEM = 50000000

	END MODULE
