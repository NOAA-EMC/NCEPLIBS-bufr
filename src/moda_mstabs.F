C> @file
C> @brief Declare arrays for internal storage of master Table B
C> and Table D entries.
C> @author J. Ator @date 2014-12-10

C> This module contains array and variable declarations used to
C> store master Table B and Table D entries within internal
C> memory.
C>
C> Data values within this module are stored by subroutine
C> ireadmt().
C>
C> @author J. Ator
C> @date 2014-12-10

        MODULE MODA_MSTABS

C>        Number of master Table B entries (up to a maximum of MXMTBB).
          INTEGER :: NMTB
C>
C>        Number of master Table D entries (up to a maximum of MXMTBD).
          INTEGER :: NMTD
C>
C>        Bit-wise representations of FXY numbers for master Table B.
          INTEGER, ALLOCATABLE :: IBFXYN(:)
C>
C>        Scale factors corresponding to ibfxyn.
          CHARACTER, ALLOCATABLE :: CBSCL(:,:)
C>
C>        Reference values corresponding to ibfxyn.
          CHARACTER, ALLOCATABLE :: CBSREF(:,:)
C>
C>        Bit widths corresponding to ibfxyn.
          CHARACTER, ALLOCATABLE :: CBBW(:,:)
C>
C>        Units corresponding to ibfxyn.
          CHARACTER, ALLOCATABLE :: CBUNIT(:,:)
C>
C>        Mnemonics corresponding to ibfxyn.
          CHARACTER, ALLOCATABLE :: CBMNEM(:,:)
C>
C>        Element names corresponding to ibfxyn.
          CHARACTER, ALLOCATABLE :: CBELEM(:,:)
C>
C>        Bit-wise representations of FXY numbers for master Table D.
          INTEGER, ALLOCATABLE :: IDFXYN(:)
C>
C>        Sequence names corresponding to idfxyn.
          CHARACTER, ALLOCATABLE :: CDSEQ(:,:)
C>
C>        Mnemonics corresponding to idfxyn.
          CHARACTER, ALLOCATABLE :: CDMNEM(:,:)
C>
C>        Numbers of child descriptors corresponding to idfxyn.
          INTEGER, ALLOCATABLE :: NDELEM(:)
C>
C>        Bit-wise representations of child descriptors corresponding
C>        to idfxyn.
          INTEGER, ALLOCATABLE :: IDEFXY(:)

        END MODULE
