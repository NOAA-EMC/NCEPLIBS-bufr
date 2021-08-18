C> @file
C> @brief Declare and initialize MXLCC variable.

C> This module declares and initializes the MXLCC variable.
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

	MODULE MODV_MXLCC

C>        @var mxlcc
C>        Maximum length (in bytes) of a character string that can be
C>        written into a data subset of a compressed BUFR message by
C>        the BUFRLIB software.

	  INTEGER :: MXLCC = 32

	END MODULE
