C> @file
C> @brief Declare and initialize MXH4WLC variable.

C> This module declares and initializes the MXH4WLC variable.
C>
C> @author J. Ator
C> @date 2014-12-10

	MODULE MODV_MXH4WLC

C>        @var mxh4wlc
C>        Maximum number of long character strings that can be held
C>        for writing into an uncompressed BUFR subset by future
C>        internal calls to subroutine writlc().

	  INTEGER, PARAMETER :: MXH4WLC = 10

	END MODULE
