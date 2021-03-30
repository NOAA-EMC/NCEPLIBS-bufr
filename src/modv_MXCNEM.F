C> @file
C> @brief Declare and initialize MXCNEM variable.

C> This module declares and initializes the MXCNEM variable.
C>
C> @author J. Ator
C> @date 2021-03-24

	MODULE MODV_MXCNEM

C>        @var mxcnem
C>        Maximum number of entries in the internal Table A
C>        mnemonic cache that is used for Section 3 decoding
C>        of BUFR messages.

	  INTEGER, PARAMETER :: MXCNEM = 450

	END MODULE
