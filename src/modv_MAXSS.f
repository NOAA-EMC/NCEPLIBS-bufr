C> @file
C> @brief Declare and initialize MAXSS variable.

C> This module declares and initializes the MAXSS variable.
C>
C> <p>This variable is initialized to a default value which can
C> be overridden by a subsequent call to function isetprm() within
C> the application program.
C>
C> @author J. Ator
C> @date 2014-12-10

        MODULE MODV_MAXSS

C>        @var maxss
C>        Maximum number of data values that can be read from or
C>        written into a data subset by the BUFRLIB software.

          INTEGER :: MAXSS = 120000

        END MODULE
