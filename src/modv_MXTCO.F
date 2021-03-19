C> @file
C> @brief Declare and initialize MXTCO variable.

C> This module declares and initializes the MXTCO variable.
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

	MODULE MODV_MXTCO

C>        @var mxtco
C>        Maximum number of Table C operators with an XX value
C>        of 21 or greater that can appear within the data subset
C>        definition of a Table A mnemonic.

#ifdef STATIC_ALLOCATION
	  INTEGER, PARAMETER :: MXTCO = 30
#else
	  INTEGER :: MXTCO = 30
#endif

	END MODULE
