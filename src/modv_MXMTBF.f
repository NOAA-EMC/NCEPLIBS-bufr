C> @file
C> @brief Declare and initialize MXMTBF variable.

C> This module declares and initializes the MXMTBF variable.
C>
C> <p>This variable is initialized to a default value which can
C> be overridden by a subsequent call to function isetprm() within
C> the application program.
C>
C> @author J. Ator
C> @date 2018-01-11

        MODULE MODV_MXMTBF

C>        @var mxmtbf
C>        Maximum number of entries in a master BUFR Code/Flag
C>        table, counting across all individual Code/Flag tables,
C>        and counting each defined code figure (within each
C>        individual Code table) or defined bit number (within
C>        each individual Flag table) as a separate entry.

          INTEGER :: MXMTBF = 25000

        END MODULE
