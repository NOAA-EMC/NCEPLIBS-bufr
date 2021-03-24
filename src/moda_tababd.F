C> @file
C> @brief Declare arrays for internal storage of DX BUFR tables.

C> This module contains array and variable declarations used to
C> store DX BUFR tables internally for multiple I/O streams.
C>
C> @author J. Ator
C> @date 2014-12-10

	MODULE MODA_TABABD

#ifndef MAXTBA_H
#define MAXTBA_H
	  USE MODV_MAXTBA
#endif

#ifndef MAXTBB_H
#define MAXTBB_H
	  USE MODV_MAXTBB
#endif

#ifndef MAXTBD_H
#define MAXTBD_H
	  USE MODV_MAXTBD
#endif

#ifndef NFILES_H
#define NFILES_H
	  USE MODV_NFILES
#endif

C>        @var ntba
C>        Number of Table A entries for each internal I/O stream
C>        (up to a maximum of MAXTBA, whose value is stored in
C>        array element 0).
C>
C>        @var taba
C>        Table A entries for each internal I/O stream.
C>
C>        @var idna
C>        Message types (in array element 1) and subtypes (in array
C>        element 2) corresponding to taba.
C>
C>        @var mtab
C>        Entries within jump/link table corresponding to taba.
C>
C>        @var ntbb
C>        Number of Table B entries for each internal I/O stream
C>        (up to a maximum of MAXTBB, whose value is stored in
C>        array element 0).
C>
C>        @var tabb
C>        Table B entries for each internal I/O stream.
C>
C>        @var idnb
C>        Bit-wise representations of the FXY values corresponding
C>        to tabb.
C>
C>        @var ntbd
C>        Number of Table D entries for each internal I/O stream
C>        (up to a maximum of MAXTBD, whose value is stored in
C>        array element 0).
C>
C>        @var tabd
C>        Table D entries for each internal I/O stream.
C>
C>        @var idnd
C>        Bit-wise representations of the FXY values corresponding
C>        to tabd.

#ifdef DYNAMIC_ALLOCATION
	  INTEGER, ALLOCATABLE :: NTBA(:)
	  INTEGER, ALLOCATABLE :: NTBB(:)
	  INTEGER, ALLOCATABLE :: NTBD(:)
	  INTEGER, ALLOCATABLE :: MTAB(:,:)
	  INTEGER, ALLOCATABLE :: IDNA(:,:,:)
	  INTEGER, ALLOCATABLE :: IDNB(:,:)
	  INTEGER, ALLOCATABLE :: IDND(:,:)
	  CHARACTER*128, ALLOCATABLE :: TABA(:,:)
	  CHARACTER*128, ALLOCATABLE :: TABB(:,:)
	  CHARACTER*600, ALLOCATABLE :: TABD(:,:)
#else
	  INTEGER :: NTBA(0:NFILES)
	  INTEGER :: NTBB(0:NFILES)
	  INTEGER :: NTBD(0:NFILES)
	  INTEGER :: MTAB(MAXTBA,NFILES)
	  INTEGER :: IDNA(MAXTBA,NFILES,2)
	  INTEGER :: IDNB(MAXTBB,NFILES)
	  INTEGER :: IDND(MAXTBD,NFILES)
	  CHARACTER*128 :: TABA(MAXTBA,NFILES)
	  CHARACTER*128 :: TABB(MAXTBB,NFILES)
	  CHARACTER*600 :: TABD(MAXTBD,NFILES)
#endif

	END MODULE
