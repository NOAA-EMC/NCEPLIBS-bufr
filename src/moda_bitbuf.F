C> @file
C> @brief Declare arrays for internal storage of BUFR messages.

C> This module contains array and variable declarations used to
C> store BUFR messages internally for multiple I/O streams.
C>
C> @author J. Ator
C> @date 2014-12-10

	MODULE MODA_BITBUF

#ifndef MXMSGL_H
#define MXMSGL_H
	  USE MODV_MXMSGL
#endif

#ifndef NFILES_H
#define NFILES_H
	  USE MODV_NFILES
#endif

C>        @var maxbyt
C>        Maximum length of an output BUFR message.
C>
C>        @var mbyt
C>        Length (in bytes) of current BUFR message for each internal
C>        I/O stream.
C>
C>        @var mbay
C>        Current BUFR message for each internal I/O stream.

	  INTEGER :: MAXBYT
	  INTEGER :: IBIT
#ifdef DYNAMIC_ALLOCATION
	  INTEGER, ALLOCATABLE :: IBAY(:)
	  INTEGER, ALLOCATABLE :: MBYT(:)
	  INTEGER, ALLOCATABLE :: MBAY(:,:)
#else
	  INTEGER :: IBAY(MXMSGLD4)
	  INTEGER :: MBYT(NFILES)
	  INTEGER :: MBAY(MXMSGLD4,NFILES)
#endif

	END MODULE
