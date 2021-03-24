C> @file
C> @brief Declare and initialize NFILES variable.

C> This module declares and initializes the NFILES variable.
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

	MODULE MODV_NFILES

C>	  @var nfiles
C>        Maximum number of BUFR files that can be connected to
C>        the BUFRLIB software (for reading or writing) at any
C>        one time.

#ifdef STATIC_ALLOCATION
	  INTEGER, PARAMETER :: NFILES = 32
#else
	  INTEGER :: NFILES = 32
#endif

	END MODULE
