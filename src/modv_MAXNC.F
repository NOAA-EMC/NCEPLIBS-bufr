C> @file
C> @brief Declare and initialize MAXNC variable.

C> This module declares and initializes the MAXNC variable.
C>
C> @author J. Ator
C> @date 2021-03-24

	MODULE MODV_MAXNC

C>        @var maxnc
C>        Maximum number of descriptors within Section 3 of
C>        a BUFR message. 

	  INTEGER, PARAMETER :: MAXNC = 600

	END MODULE
