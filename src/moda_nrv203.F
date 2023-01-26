C> @file
C> @brief Declare arrays for internal storage of changed
C> reference values.
C> @author J. Ator @date 2012-03-02

C> This module contains array and variable declarations for use
C> with any 2-03-YYY (change reference value) operators present
C> within the internal jump/link table.
C>
C> Data values within this module are stored by subroutine
C> tabsub().
C>
C> @author J. Ator
C> @date 2012-03-02

	MODULE MODA_NRV203

C>        Number of entries in the jump/link table which contain
C>        new reference values (up to a maximum of MXNRV).
	  INTEGER :: NNRV
C>
C>        Number of bits in Section 4 occupied by each new
C>        reference value for the current 2-03-YYY operator in
C>        scope; set to 0 if no such operator is currently in
C>        scope.
	  INTEGER :: IBTNRV
C>
C>        A number between 1 and nnrv, denoting the first entry
C>        within the module arrays which applies to the current
C>        data subset in scope; set to 0 if no 2-03-YYY operators
C>        have been applied to the current data subset in scope. 
	  INTEGER :: IPFNRV
C>
C>        Table B mnemonic to which the corresponding new
C>        reference value in nrv applies.
	  CHARACTER*8, ALLOCATABLE :: TAGNRV(:)
C>
C>        Entries within jump/link table which contain new
C>        reference values.
	  INTEGER    , ALLOCATABLE :: INODNRV(:) 
C>
C>        New reference values corresponding to inodnrv.
	  INTEGER*8  , ALLOCATABLE :: NRV(:) 
C>
C>        Start of entry range in jump/link table, within which
C>        the corresponding new reference value in nrv will be
C>        applied to all occurrences of the corresponding
C>        Table B mnemonic in tagnrv.
	  INTEGER    , ALLOCATABLE :: ISNRV(:) 
C>
C>        End of entry range in jump/link table, within which
C>        the corresponding new reference value in nrv will be
C>        applied to all occurrences of the corresponding
C>        Table B mnemonic in tagnrv.
	  INTEGER    , ALLOCATABLE :: IENRV(:) 

	END MODULE
