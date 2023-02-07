C> @file
C> @brief Declare and initialize MXTAMC variable.

C> This module declares and initializes the MXTAMC variable.
C>
C> <p>This variable is initialized to a default value which can
C> be overridden by a subsequent call to function isetprm() within
C> the application program.
C>
C> @author J. Ator
C> @date 2014-12-10

        MODULE MODV_MXTAMC

C>        @var mxtamc
C>        Maximum number of Table A mnemonics in the internal
C>        jump/link table which contain at least one Table C operator
C>        with an XX value of 21 or greater in their definition.

          INTEGER :: MXTAMC = 15

        END MODULE
