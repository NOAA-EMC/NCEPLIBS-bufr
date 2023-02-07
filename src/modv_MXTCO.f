C> @file
C> @brief Declare and initialize MXTCO variable.

C> This module declares and initializes the MXTCO variable.
C>
C> <p>This variable is initialized to a default value which can
C> be overridden by a subsequent call to function isetprm() within
C> the application program.
C>
C> @author J. Ator
C> @date 2014-12-10

        MODULE MODV_MXTCO

C>        @var mxtco
C>        Maximum number of Table C operators with an XX value
C>        of 21 or greater that can appear within the data subset
C>        definition of a Table A mnemonic.

          INTEGER :: MXTCO = 30

        END MODULE
