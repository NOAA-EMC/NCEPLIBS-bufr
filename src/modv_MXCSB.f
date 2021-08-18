C> @file
C> @brief Declare and initialize MXCSB variable.

C> This module declares and initializes the MXCSB variable.
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

	MODULE MODV_MXCSB

C>        @var mxcsb
C>        Maximum number of data subsets that can be written into
C>	  a compressed BUFR message by the BUFRLIB software.

	  INTEGER :: MXCSB = 4000

	END MODULE
