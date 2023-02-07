C> @file
C> @brief Declare arrays for internal storage of output switches.
C>
C> @author J. Woollen @date 2003-11-04

C> This module contains an array declaration used to store a switch for
C> each internal I/O stream index, indicating whether any BUFR messages should
C> actually be written to the corresponding logical unit.
C>
C> The default value is "on", indicating that BUFR messages should be
C> written to the corresponding logical unit, but it can be switched
C> to a value of "off" if it is connected to the library via a call to
C> subroutine openbf() with IO = 'NUL', and in which case no output will
C> actually be written to the corresponding logical unit.  This can be
C> useful if, for example, an application program is planning to use
C> subroutine writsa() to return output BUFR messages via a memory array
C> and does not want to also have them automatically written out to a file.
C>
C> @author J. Woollen @date 2003-11-04

        MODULE MODA_NULBFR

C>    Output switch for each internal I/O stream index:
C>    - 0 BUFR messages will be written to corresponding logical unit (default)
C>    - 1 no BUFR messages will be written to corresponding logical unit
          INTEGER, ALLOCATABLE :: NULL(:)

        END MODULE
