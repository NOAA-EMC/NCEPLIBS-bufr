C> @file
C> @brief Declare and initialize MXNAF variable.

C> This module declares and initializes the MXNAF variable.
C>
C> @author J. Ator
C> @date 2021-03-24

	MODULE MODV_MXNAF

C>        @var mxnaf
C>        Maximum number of associated fields that can be in
C>        effect at any given time for a Table B descriptor.

	  INTEGER, PARAMETER :: MXNAF = 4

	END MODULE
