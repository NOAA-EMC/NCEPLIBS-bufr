C> @file
C> @brief Declare arrays for internal storage of jump/link table.

C> This module contains array and variable declarations used to
C> store the internal jump/link table.
C>
C> <p>Data values within this module are stored by subroutines
C> makestab(), tabsub() and tabent().
C>
C> @author J. Ator
C> @date 2014-12-10

	MODULE MODA_TABLES

#ifndef MAXJL_H
#define MAXJL_H
	  USE MODV_MAXJL
#endif

C>        @var maxtab
C>        Maximum number of entries in the jump/link table;
C>        equivalent to MAXJL.
C>
C>        @var ntab
C>        Number of entries in the jump/link table.
C>
C>        @var tag
C>        Mnemonics in the jump/link table.
C>
C>        @var typ
C>        Type indicators corresponding to tag:
C>        - "SUB", if corresponding tag entry is a Table A mnemonic
C>        - "SEQ", if corresponding tag entry is a Table D mnemonic
C>          using either short (1-bit) delayed replication, regular
C>          (non-delayed) replication, or no replication at all
C>        - "RPC", if corresponding tag entry is a Table D mnemonic
C>          using either medium (8-bit) or long (16-bit) delayed
C>          replication
C>        - "RPS", if corresponding tag entry is a Table D mnemonic
C>          using medium (8-bit) delayed replication in a stack
C>        - "DRB", if corresponding tag entry denotes the short (1-bit)
C>          delayed replication of a Table D mnemonic
C>        - "DRP", if corresponding tag entry denotes the medium (8-bit)
C>          or long (16-bit) delayed replication of a Table D mnemonic
C>        - "DRS", if corresponding tag entry denotes the medium (8-bit)
C>          delayed replication of a Table D mnemonic in a stack
C>        - "REP", if corresponding tag entry denotes the regular
C>          (non-delayed) replication of a Table D mnemonic
C>        - "CHR", if corresponding tag entry is a Table B mnemonic
C>          with units of CCITT IA5
C>        - "NUM", if corresponding tag entry is a Table B mnemonic
C>          with any units other than CCITT IA5
C>
C>        @var jump
C>        Jump forward indices corresponding to tag and typ:
C>        - 0, if corresponding typ entry is "CHR" or "NUM"
C>        - Jump/link table entry for Table D mnemonic whose replication
C>          is denoted by corresponding tag entry, if corresponding typ
C>          entry is "DRB", "DRP" or "REP"
C>        - Jump/link table entry for Table B or D mnemonic which is
C>          the first sequential child descriptor of the corresponding
C>          tag entry, otherwise
C>
C>        @var jmpb
C>        Jump backward indices corresponding to tag and typ:
C>        - 0, if corresponding typ entry is "SUB"
C>        - Jump/link table entry denoting the replication of
C>          corresponding tag entry, if corresponding typ entry is
C>          "RPC", or if corresponding typ entry is "SEQ" and 
C>          corresponding tag entry uses either short (1-bit) or
C>          regular (non-delayed) replication
C>        - Jump/link table entry for Table A or D mnemonic of
C>          which corresponding tag entry is a child descriptor,
C>          otherwise
C>
C>        @var link
C>        Link indices corresponding to tag, typ and jmpb:
C>        - 0, if corresponding typ entry is "SUB" or "RPC", or if
C>          corresponding typ entry is "SEQ" and corresponding tag
C>          entry uses either short (1-bit) or regular (non-delayed)
C>          replication, or if corresponding tag entry is the last
C>          sequential child descriptor of the Table A or D
C>          mnemonic referenced by corresponding jmpb entry
C>        - Jump/link table entry for Table B or D mnemonic which
C>          follows the corresponding tag entry as the next sequential
C>          child descriptor of the Table A or D mnemonic referenced 
C>          by corresponding jmpb entry, otherwise
C>          
C>        @var ibt
C>        Bit widths corresponding to tag and typ:
C>        - Bit width of corresponding tag entry, if corresponding
C>          typ entry is "CHR", "NUM", "DRB" or "DRP"
C>        - 0, otherwise
C>
C>        @var irf
C>        Reference values corresponding to tag and typ:
C>        - Reference value of corresponding tag entry, if
C>          corresponding typ entry is "NUM"
C>        - Number of regular (non-delayed) replications of Table D
C>          mnemonic referenced by corresponding jump entry, if
C>          corresponding typ entry is "REP"
C>        - 0, otherwise
C>
C>        @var isc
C>        Scale factors corresponding to tag and typ:
C>        - Scale factor of corresponding tag entry, if
C>          corresponding typ entry is "NUM"
C>        - Jump/link table entry for Table B or Table D mnemonic
C>          which is the last sequential child descriptor of the
C>          corresponding tag entry, if the corresponding typ entry
C>          is "SUB"
C>        - 0, otherwise
C>
C>        @var itp
C>        Integer type values corresponding to typ:
C>        - 1, if corresponding typ entry is "DRS", "DRP" or "DRB"
C>        - 2, if corresponding typ entry is "NUM"
C>        - 3, if corresponding typ entry is "CHR"
C>        - 0, otherwise
C>
C>        @var vali
C>        Initialized data values corresponding to typ:
C>        - Current placeholder value for "missing" data, if
C>          corresponding typ entry is "REP", "NUM" or "CHR"
C>        - 0, otherwise
C>
C>        @var knti
C>        Initialized replication counts corresponding to typ and jump:
C>        - 0, if corresponding typ entry is "RPC", "RPS" or "DRB"
C>        - Number of regular (non-delayed) replications of Table D
C>          mnemonic referenced by corresponding jump entry, if
C>          corresponding typ entry is "REP"
C>        - 1, otherwise

	  INTEGER :: MAXTAB
	  INTEGER :: NTAB
#ifdef DYNAMIC_ALLOCATION
	  CHARACTER*10, ALLOCATABLE :: TAG(:)
	  CHARACTER*3, ALLOCATABLE :: TYP(:)
	  INTEGER, ALLOCATABLE :: KNT(:)
	  INTEGER, ALLOCATABLE :: JUMP(:) 
	  INTEGER, ALLOCATABLE :: LINK(:)
	  INTEGER, ALLOCATABLE :: JMPB(:)
	  INTEGER, ALLOCATABLE :: IBT(:)
	  INTEGER, ALLOCATABLE :: IRF(:)
	  INTEGER, ALLOCATABLE :: ISC(:)
	  INTEGER, ALLOCATABLE :: ITP(:)
	  REAL*8, ALLOCATABLE :: VALI(:)
	  INTEGER, ALLOCATABLE :: KNTI(:)
	  INTEGER, ALLOCATABLE :: ISEQ(:,:)
	  INTEGER, ALLOCATABLE :: JSEQ(:)
#else
	  CHARACTER*10 :: TAG(MAXJL)
	  CHARACTER*3 :: TYP(MAXJL)
	  INTEGER :: KNT(MAXJL)
	  INTEGER :: JUMP(MAXJL) 
	  INTEGER :: LINK(MAXJL)
	  INTEGER :: JMPB(MAXJL)
	  INTEGER :: IBT(MAXJL)
	  INTEGER :: IRF(MAXJL)
	  INTEGER :: ISC(MAXJL)
	  INTEGER :: ITP(MAXJL)
	  REAL*8 :: VALI(MAXJL)
	  INTEGER :: KNTI(MAXJL)
	  INTEGER :: ISEQ(MAXJL,2)
	  INTEGER :: JSEQ(MAXJL)
#endif

	END MODULE
