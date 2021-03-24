C> @file
C> @brief Declare and initialize MXMSGL variable.

C> This module declares and initializes the MXMSGL variable. 
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

	MODULE MODV_MXMSGL

C>        @var mxmsgl
C>        Maximum length (in bytes) of a BUFR message that can be
C>        read or written by the BUFRLIB software.

#ifdef STATIC_ALLOCATION
	  INTEGER, PARAMETER :: MXMSGL = 600000
	  INTEGER, PARAMETER :: MXMSGLD4 = MXMSGL/4
#else
	  INTEGER :: MXMSGL = 200000
	  INTEGER :: MXMSGLD4
#endif

	END MODULE
