C> @file
C> @brief Declare and initialize MXMTBF variable.

C> This module declares and initializes the MXMTBF variable.
C>
C> <p>For dynamic allocation builds, this variable is initialized
C> to a default value which can be overridden by a subsequent call
C> to function isetprm() within the application program.
C> For static allocation builds, this variable is set as a
C> parameter at compile time and cannot be changed within the
C> application program.
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
