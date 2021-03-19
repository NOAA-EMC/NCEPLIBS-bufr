C> @file
C> @brief Declare and initialize MAXSS variable.

C> This module declares and initializes the MAXSS variable.
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

	MODULE MODV_MAXSS

C>        @var maxss
C>        Maximum number of data values that can be read from or
C>        written into a data subset by the BUFRLIB software.

#ifdef STATIC_ALLOCATION
	  INTEGER, PARAMETER :: MAXSS = 120000
#else
	  INTEGER :: MAXSS = 80000
#endif

	END MODULE
