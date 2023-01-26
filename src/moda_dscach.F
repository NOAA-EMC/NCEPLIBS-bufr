C> @file
C> @brief Declare arrays for internal storage of the Table A
C> mnemonic cache.
C> @author J. Ator @date 2012-03-02

C> This module contains array and variable declarations for the
C> internal Table A mnemonic cache that is used for Section 3
C> decoding of BUFR messages.
C>
C> Data values within this module are stored by subroutine reads3().
C>
C> @author J. Ator
C> @date 2012-03-02

	MODULE MODA_DSCACH

	  USE MODV_MAXNC
	  USE MODV_MXCNEM

C>    Number of entries in the internal Table A mnemonic
C>    cache (up to a maximum of MXCNEM).
	  INTEGER :: NCNEM
C>    Table A mnemonics.
	  CHARACTER*8 :: CNEM(MXCNEM)
C>    Number of child descriptors for the corresponding
C>    Table A mnemonic in cnem.
	  INTEGER :: NDC(MXCNEM)
C>    Bit-wise representations of the child descriptors
C>    for the corresponding Table A mnemonic in cnem.
	  INTEGER :: IDCACH(MXCNEM,MAXNC)

	END MODULE
