C> @file
C> @brief Declare arrays for internal storage of bitmaps.

C> This module contains array and variable declarations used to
C> store bitmaps internally within a data subset definition.
C>
C> <p>Data values within this module are stored by subprograms
C> strbtm(), igetrfel(), makestab() and tabsub().
C>
C> @author J. Ator
C> @date 2016-05-27

	MODULE MODA_BITMAPS

C>        @var nbtm
C>        Number of stored bitmaps for the current data subset
C>        (up to a maximum of MXBTM).
C>
C>        @var nbtmse
C>        Number of "set" entries (set to a value of 0)
C>        in the bitmap.
C>
C>        @var linbtm
C>        TRUE if a bitmap is in the process of being read for
C>        the current data subset.
C>
C>        @var istbtm
C>        Ordinal position in data subset definition corresponding
C>        to the first entry of the bitmap.
C>
C>        @var iszbtm
C>        Size of bitmap (total number of entries, whether
C>        "set" (set to a value of 0) or not).
C>
C>        @var ibtmse
C>        Ordinal positions in bitmap of bits that were "set" (set
C>        to a value of 0); these ordinal positions can range in
C>        value from 1 to iszbtm for each stored bitmap.
C>
C>        @var lstnod
C>        Most recent jump/link table entry that was processed by
C>        function igetrfel() and whose corresponding value type
C>        was either numeric or CCITT IA5.
C>
C>        @var lstnodct
C>        Current count of consecutive occurrences of lstnod.
C>        
C>        @var ntamc
C>        Number of Table A mnemonics in jump/link table (up to a
C>        maximum of MXTAMC) which contain at least one Table C
C>        operator with an XX value of 21 or greater in their data
C>        subset definition; only Table C operators with an XX
C>        value of 21 or greater are tracked within this module,
C>        since all others are automatically processed within
C>        subroutines tabsub() and tabent().
C>
C>        @var inodtamc
C>        Entries within jump/link table which contain Table A
C>        mnemonics.
C>
C>        @var ntco
C>        Number of Table C operators (with an XX value of 21 or
C>        greater) within the data subset definition of the
C>        corresponding Table A mnemonic in inodtamc.
C>
C>        @var ctco
C>        Table C operators corresponding to inodtco.
C>
C>        @var inodtco
C>        Entries within jump/link table which contain Table C
C>        operators.

	  INTEGER :: NBTM
	  INTEGER :: NTAMC
	  INTEGER :: LSTNOD
	  INTEGER :: LSTNODCT
	  LOGICAL :: LINBTM
	  INTEGER, ALLOCATABLE :: INODTAMC(:) 
	  INTEGER, ALLOCATABLE :: NTCO(:) 
	  CHARACTER*6, ALLOCATABLE :: CTCO(:,:)
	  INTEGER, ALLOCATABLE :: INODTCO(:,:) 
	  INTEGER, ALLOCATABLE :: NBTMSE(:)
	  INTEGER, ALLOCATABLE :: ISTBTM(:) 
	  INTEGER, ALLOCATABLE :: ISZBTM(:)
	  INTEGER, ALLOCATABLE :: IBTMSE(:,:)

	END MODULE
