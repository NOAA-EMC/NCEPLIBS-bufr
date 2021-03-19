C> @file
C> @brief Declare and initialize MXTAMC variable.

C> This module declares and initializes the MXTAMC variable.
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

	MODULE MODV_MXTAMC

C>        @var mxtamc
C>        Maximum number of Table A mnemonics in the internal
C>        jump/link table which contain at least one Table C operator
C>        with an XX value of 21 or greater in their definition.

#ifdef STATIC_ALLOCATION
	  INTEGER, PARAMETER :: MXTAMC = 15
#else
	  INTEGER :: MXTAMC = 15
#endif

	END MODULE
