C> @file
C> @brief Declare arrays for internal storage of Section 0 and 1
C> mnemonics and values.
C>
C> @author J. Ator @date 2015-03-03

C> This module contains array and variable declarations used to store
C> custom values for certain mnemonics within Sections 0 and 1
C> of all future output BUFR messages written to all Fortran logical units.
C> These values are specified via one or more calls to subroutine pkvs01().
C> Otherwise, default values for these mnemonics are used as defined
C> within subroutines msgini(), cmsgini(), or dxmini().
C>
C> @author J. Ator @date 2015-03-03

        MODULE MODA_S01CM

C>    Custom values for use within Sections 0 and 1 of all future
C>    output BUFR messages written to all Fortran logical units.
          INTEGER, ALLOCATABLE :: IVMNEM(:)
C>    Section 0 and 1 mnemonics corresponding to ivmnem.
          CHARACTER*8, ALLOCATABLE :: CMNEM(:)
C>    Number of custom values stored.
          INTEGER :: NS01V = 0

        END MODULE
