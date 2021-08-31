C> @file
C> @brief Declare and initialize MXCSB variable.

C> This module declares and initializes the MXCSB variable.
C>
C> <p>This variable is initialized to a default value which can
C> be overridden by a subsequent call to function isetprm() within
C> the application program.
C>
C> @author J. Ator
C> @date 2014-12-10

	MODULE MODV_MXCSB

C>        @var mxcsb
C>        Maximum number of data subsets that can be written into
C>	  a compressed BUFR message by the BUFRLIB software.

	  INTEGER :: MXCSB = 4000

	END MODULE
