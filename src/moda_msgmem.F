C> @file
C> @brief Declare arrays for internal storage of BUFR files.
C> @author J. Ator @date 2014-12-10

C> This module contains array and variable declarations used to
C> store the contents of one or more BUFR files within internal
C> memory.
C>
C> Data values within this module are stored by subroutines
C> ufbmem() and cpdxmm().
C>
C> @author J. Ator
C> @date 2014-12-10

	MODULE MODA_MSGMEM

C>        Fortran logical unit number for use in accessing
C>        contents of BUFR files within internal memory.
	  INTEGER :: MUNIT
C>        Number of array elements filled within msgs (up
C>        to a maximum of MAXMEM).
	  INTEGER :: MLAST
C>        Number of array elements filled within mdx (up
C>        to a maximum of MXDXW).
	  INTEGER :: LDXM
C>        Number of DX BUFR table messages stored within mdx
C>        (up to a maximum of MXDXM).
	  INTEGER :: NDXM
C>        Number of DX BUFR table that is currently in scope,
C>        depending on which BUFR message within msgs is
C>        currently in scope from the most recent call to
C>        subroutine rdmemm() or readmm().
	  INTEGER :: LDXTS
C>        Number of DX BUFR tables represented by the
C>        messages within mdx (up to a maximum of MXDXTS).
	  INTEGER :: NDXTS
C>        ??? description for MXDXM
	  INTEGER :: MXDXM
C>        ??? description for MXDXW
	  INTEGER :: MXDXW
C>        Pointers to the beginning of each message within
C>        msgs (up to a maximum of MAXMSG, and where array
C>        element 0 contains the actual number of messages
C>        stored within msgs).
	  INTEGER, ALLOCATABLE :: MSGP(:)
C>        BUFR messages read from one or more BUFR files.
	  INTEGER, ALLOCATABLE :: MSGS(:)
C>        DX BUFR table messages read from one or more BUFR
C>        files, for use in decoding the messages in msgs.
	  INTEGER, ALLOCATABLE :: MDX(:)
C>        Pointers to the beginning of each message within mdx.
	  INTEGER, ALLOCATABLE :: IPDXM(:)
C>        Pointers to the beginning of each DX BUFR table
C>        within mdx.
	  INTEGER, ALLOCATABLE :: IFDXTS(:)
C>        Number of consecutive messages within mdx which
C>        constitute each DX BUFR table, beginning with the
C>        corresponding ifdxts.
	  INTEGER, ALLOCATABLE :: ICDXTS(:)
C>        Pointers to first message within msgs for which each
C>        DX BUFR table applies.
	  INTEGER, ALLOCATABLE :: IPMSGS(:)

	END MODULE
