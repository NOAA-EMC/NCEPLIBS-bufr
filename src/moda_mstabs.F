C> @file
C> @brief Declare arrays for internal storage of master Table B
C> and Table D entries.

C> This module contains array and variable declarations used to
C> store master Table B and Table D entries within internal
C> memory.
C>
C> <p>Data values within this module are stored by subroutine
C> ireadmt().
C>
C> @author J. Ator
C> @date 2014-12-10

	MODULE MODA_MSTABS

#ifndef MAXCD_H
#define MAXCD_H
	  USE MODV_MAXCD
#endif

#ifndef MXMTBB_H
#define MXMTBB_H
	  USE MODV_MXMTBB
#endif

#ifndef MXMTBD_H
#define MXMTBD_H
	  USE MODV_MXMTBD
#endif

C>        @var nmtb
C>        Number of master Table B entries (up to a maximum of MXMTBB).
C>
C>        @var ibfxyn
C>        Bit-wise representations of FXY numbers for master Table B.
C>
C>        @var cbscl
C>        Scale factors corresponding to ibfxyn.
C>
C>        @var cbsref
C>        Reference values corresponding to ibfxyn.
C>
C>        @var cbbw
C>        Bit widths corresponding to ibfxyn.
C>
C>        @var cbunit
C>        Units corresponding to ibfxyn.
C>
C>        @var cbmnem
C>        Mnemonics corresponding to ibfxyn.
C>
C>        @var cbelem
C>        Element names corresponding to ibfxyn.
C>
C>        @var nmtd
C>        Number of master Table D entries (up to a maximum of MXMTBD).
C>
C>        @var idfxyn
C>        Bit-wise representations of FXY numbers for master Table D.
C>
C>        @var cdseq
C>        Sequence names corresponding to idfxyn.
C>
C>        @var cdmnem
C>        Mnemonics corresponding to idfxyn.
C>
C>        @var ndelem
C>        Numbers of child descriptors corresponding to idfxyn.
C>
C>        @var idefxy
C>        Bit-wise representations of child descriptors corresponding
C>        to idfxyn.

	  INTEGER :: NMTB
	  INTEGER :: NMTD
	  INTEGER, ALLOCATABLE :: IBFXYN(:)
	  CHARACTER, ALLOCATABLE :: CBSCL(:,:)
	  CHARACTER, ALLOCATABLE :: CBSREF(:,:)
	  CHARACTER, ALLOCATABLE :: CBBW(:,:)
	  CHARACTER, ALLOCATABLE :: CBUNIT(:,:)
	  CHARACTER, ALLOCATABLE :: CBMNEM(:,:)
	  CHARACTER, ALLOCATABLE :: CBELEM(:,:)
	  INTEGER, ALLOCATABLE :: IDFXYN(:)
	  CHARACTER, ALLOCATABLE :: CDSEQ(:,:)
	  CHARACTER, ALLOCATABLE :: CDMNEM(:,:)
	  INTEGER, ALLOCATABLE :: NDELEM(:)
	  INTEGER, ALLOCATABLE :: IDEFXY(:)

	END MODULE
