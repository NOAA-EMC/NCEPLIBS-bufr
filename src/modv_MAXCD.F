C> @file
C> @brief Declare and initialize MAXCD variable.

C> This module declares and initializes the MAXCD variable.
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

	MODULE MODV_MAXCD

C>        @var maxcd
C>	  Maximum number of child descriptors that can be included
C>        within the sequence definition of a Table D descriptor,
C>        not counting the recursive resolution of any child
C>        descriptors which may themselves be Table D descriptors.

#ifdef STATIC_ALLOCATION
	  INTEGER, PARAMETER :: MAXCD = 250
#else
	  INTEGER :: MAXCD = 250
#endif

	END MODULE
