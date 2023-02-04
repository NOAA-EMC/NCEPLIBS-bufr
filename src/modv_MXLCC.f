C> @file
C> @brief Declare and initialize MXLCC variable.

C> This module declares and initializes the MXLCC variable.
C>
C> <p>This variable is initialized to a default value which can
C> be overridden by a subsequent call to function isetprm() within
C> the application program.
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
