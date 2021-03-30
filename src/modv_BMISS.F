C> @file
C> @brief Declare and initialize BMISS variable.

C> This module declares and initializes the BMISS variable.
C>
C> @author J. Ator
C> @date 2021-03-24

	MODULE MODV_BMISS

C>        @var bmiss
C>        Current placeholder value to represent "missing"
C>        data when reading from or writing to BUFR files;
C>        this value can be changed at any time via a call
C>        to subroutine setbmiss().

	  REAL*8 :: BMISS = 10E10_8

	END MODULE
