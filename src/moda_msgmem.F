C> @file
C> @brief Declare arrays for internal storage of BUFR files.

C> This module contains array and variable declarations used to
C> store the contents of one or more BUFR files within internal
C> memory.
C>
C> <p>Data values within this module are stored by subroutines
C> ufbmem() and cpdxmm().
C>
C> @author J. Ator
C> @date 2014-12-10

	MODULE MODA_MSGMEM

#ifndef MAXMSG_H
#define MAXMSG_H
	  USE MODV_MAXMSG
#endif

#ifndef MAXMEM_H
#define MAXMEM_H
	  USE MODV_MAXMEM
#endif

#ifndef MXDXTS_H
#define MXDXTS_H
	  USE MODV_MXDXTS
#endif

#ifndef MXMSGL_H
#define MXMSGL_H
	  USE MODV_MXMSGL
#endif

C>        @var munit
C>        Fortran logical unit number for use in accessing
C>        contents of BUFR files within internal memory.
C>
C>        @var msgs
C>        BUFR messages read from one or more BUFR files.
C>
C>        @var msgp
C>        Pointers to the beginning of each message within
C>        msgs (up to a maximum of MAXMSG, and where array
C>        element 0 contains the actual number of messages
C>        stored within msgs).
C>
C>        @var mlast
C>        Number of array elements filled within msgs (up
C>        to a maximum of MAXMEM).
C>
C>        @var mdx
C>        DX BUFR table messages read from one or more BUFR
C>        files, for use in decoding the messages in msgs.
C>
C>        @var ndxm
C>        Number of DX BUFR table messages stored within mdx
C>        (up to a maximum of MXDXM).
C>
C>        @var ipdxm
C>        Pointers to the beginning of each message within mdx.
C>
C>        @var ndxts
C>        Number of DX BUFR tables represented by the
C>        messages within mdx (up to a maximum of MXDXTS).
C>
C>        @var ifdxts
C>        Pointers to the beginning of each DX BUFR table
C>        within mdx.
C>
C>        @var icdxts
C>        Number of consecutive messages within mdx which
C>        constitute each DX BUFR table, beginning with the
C>        corresponding ifdxts.
C>
C>        @var ipmsgs
C>        Pointers to first message within msgs for which each
C>        DX BUFR table applies.
C>
C>        @var ldxts
C>        Number of DX BUFR table that is currently in scope,
C>        depending on which BUFR message within msgs is
C>        currently in scope from the most recent call to
C>        subroutine rdmemm() or readmm().
C>
C>        @var ldxm
C>        Number of array elements filled within mdx (up
C>        to a maximum of MXDXW).

	  INTEGER :: MUNIT
	  INTEGER :: MLAST
	  INTEGER :: LDXM
	  INTEGER :: NDXM
	  INTEGER :: LDXTS
	  INTEGER :: NDXTS
	  INTEGER :: MXDXM
	  INTEGER :: MXDXW
	  INTEGER, ALLOCATABLE :: MSGP(:)
	  INTEGER, ALLOCATABLE :: MSGS(:)
	  INTEGER, ALLOCATABLE :: MDX(:)
	  INTEGER, ALLOCATABLE :: IPDXM(:)
	  INTEGER, ALLOCATABLE :: IFDXTS(:)
	  INTEGER, ALLOCATABLE :: ICDXTS(:)
	  INTEGER, ALLOCATABLE :: IPMSGS(:)

	END MODULE
