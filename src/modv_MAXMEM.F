C> @file
C> @brief Declare and initialize MAXMEM variable.

C> This module declares and initializes the MAXMEM variable.
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

	MODULE MODV_MAXMEM

C>        @var maxmem
C>        Maximum number of bytes that can be used to store BUFR
C>        messages within internal memory.

#ifdef STATIC_ALLOCATION
	  INTEGER, PARAMETER :: MAXMEM = 50000000
#else
	  INTEGER :: MAXMEM = 50000000
#endif

	END MODULE
