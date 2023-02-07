C> @file
C> @brief Declare arrays for internal storage of temporary BUFR messages.
C>
C> @author J. Woollen @date 1994-01-06

C> This module contains a declaration for an array used by various
C> subroutines and functions to hold a temporary working copy of a
C> BUFR message.
C>
C> @author J. Woollen @date 1994-01-06

        MODULE MODA_MGWA

C>    Temporary working copy of BUFR message.
          INTEGER, ALLOCATABLE :: MGWA(:)

        END MODULE
