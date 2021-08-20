C> @file
C> @brief Declare and initialize MXS01V variable.

C> This module declares and initializes the MXS01V variable.
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

	MODULE MODV_MXS01V

C>        @var mxs01v
C>        Maximum number of default Section 0 or Section 1 values
C>        that can be overwritten within an output BUFR message by
C>        the BUFRLIB software.

	  INTEGER :: MXS01V = 10

	END MODULE
