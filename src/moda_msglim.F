C> @file
C> @brief Declare arrays for internal storage of tracking which logical
C> units should not have zero subset messages written to them.
C>
C> @author D. Keyser @date 2005-05-26

C> This module contains a declaration for an array used to keep track of
C> which logical units should not have any empty (zero data subset)
C> BUFR messages written to them.  This setting is turned on via a call to
C> subroutine closmg() with LUNIN < 0 for the logical unit in question,
C> and once set it remains in effect for the remainder of the life of the
C> application program for that particular logical unit.
C>
C> @author D. Keyser @date 2005-05-26

        MODULE MODA_MSGLIM

C>    Tracking index for each I/O stream index.  Initialized to a value of 3
C>    within subroutine bfrini(), and then reset to a value of 0 within
C>    subroutine closmg() if the corresponding logical unit should not have
C>    any empty (zero data subset) BUFR messages written to it.
C>
          INTEGER, ALLOCATABLE :: MSGLIM(:)

        END MODULE
