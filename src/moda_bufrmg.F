C> @file
C> @brief Declare arrays for internal storage of BUFR messages.
C> @author J. Ator @date 2014-12-10

C> This module contains arrays used to store, for each output I/O stream,
C> a copy of the BUFR message that was most recently written to that
C> stream, for possible later retrieval via subroutine writsa().
C>
C> @author J. Ator @date 2014-12-10

        MODULE MODA_BUFRMG

C>        Length (in integers) of BUFR message most recently written to each output I/O stream.
          INTEGER, ALLOCATABLE :: MSGLEN(:)
C>        BUFR message most recently written to each output I/O stream.
          INTEGER, ALLOCATABLE :: MSGTXT(:,:)

        END MODULE
