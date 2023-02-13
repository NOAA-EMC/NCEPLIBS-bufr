!> @file
!> @brief Declare and initialize module variables.
!>
!> @author J. Ator @date 2023-02-10

!> This module declares and initializes the BMISS variable.
!>
!> @author J. Ator @date 2021-03-24
module modv_bmiss
!>  Current placeholder value to represent "missing" data when reading from or writing to BUFR files;
!>  this value can be changed at any time via a call to subroutine setbmiss().
  real*8, public :: BMISS = 10E10_8
end module modv_bmiss

!> This module declares and initializes the IFOPBF variable.
!>
!> @author J. Ator @date 2015-03-03
module modv_ifopbf
!>  Status indicator to keep track of whether subroutine openbf() has already been called:
!>  - 0 = No
!>  - 1 = Yes
  integer, public :: IFOPBF = 0
end module modv_ifopbf

!> This module declares and initializes the IM8B variable.
!>
!> @author J. Woollen @date 2022-08-04
module modv_im8b
!>  Status indicator to keep track of whether all future calls to BUFRLIB subroutines and functions from a Fortran
!>  application program will be made using 8-byte integer arguments.
!>
!>  The default value is .false., meaning that all future calls to BUFRLIB subroutines and functions will be
!>  made using 4-byte integer arguments.  This value can be changed at any time via a call to subroutine setim8b().
  logical, public :: IM8B = .false.
end module modv_im8b

!> This module declares and initializes the MAXCD variable.
!>
!> @author J. Ator @date 2014-12-10
module modv_maxcd
!>  Maximum number of child descriptors that can be included within the sequence definition of a Table D descriptor,
!>  not counting the recursive resolution of any child descriptors which may themselves be Table D descriptors.
!>
!>  This variable is initialized to a default value which can be overridden by a subsequent call to function isetprm()
!>  within the application program.
  integer, public :: MAXCD = 250
end module modv_maxcd

!> This module declares and initializes the MAXJL variable.
!>
!> @author J. Ator @date 2014-12-10
module modv_maxjl
!>  Maximum number of entries in the internal jump/link table.
!>
!>  This variable is initialized to a default value which can be overridden by a subsequent call to function isetprm()
!>  within the application program.
  integer :: MAXJL = 96000
end module modv_maxjl

!> This module declares and initializes the MAXMEM variable.
!>
!> @author J. Ator @date 2014-12-10
module modv_maxmem
!>  Maximum number of bytes that can be used to store BUFR messages within internal memory.
!>
!>  This variable is initialized to a default value which can be overridden by a subsequent call to function isetprm()
!>  within the application program.
  integer :: MAXMEM = 50000000
end module modv_maxmem

!> This module declares and initializes the MAXMSG variable.
!>
!> @author J. Ator @date 2014-12-10
module modv_maxmsg
!>  Maximum number of BUFR messages that can be stored within internal memory.
!>
!>  This variable is initialized to a default value which can be overridden by a subsequent call to function isetprm()
!>  within the application program.
  integer :: MAXMSG = 200000
end module modv_maxmsg

!> This module declares and initializes the MAXNC variable.
!>
!> @author J. Ator @date 2021-03-24
module modv_maxnc
!>  Maximum number of descriptors within Section 3 of a BUFR message.
  integer, parameter, public :: MAXNC = 600
end module modv_maxnc

!> This module declares and initializes the MAXSS variable.
!>
!> @author J. Ator @date 2014-12-10
module modv_maxss
!>  Maximum number of data values that can be read from or written into a data subset by the BUFRLIB software.
!>
!>  This variable is initialized to a default value which can be overridden by a subsequent call to function isetprm()
!>  within the application program.
  integer :: MAXSS = 120000
end module modv_maxss

!> This module declares and initializes the MAXTBA variable.
!>
!> @author J. Ator @date 2014-12-10
module modv_maxtba
!>  Maximum number of entries in the internal BUFR Table A for each BUFR file that is connected to the BUFRLIB software.
!>
!>  This variable is initialized to a default value which can be overridden by a subsequent call to function isetprm()
!>  within the application program.
  integer :: MAXTBA = 150
end module modv_maxtba

!> This module declares and initializes the MAXTBB variable.
!>
!> @author J. Ator @date 2014-12-10
module modv_maxtbb
!>  Maximum number of entries in the internal BUFR Table B for each BUFR file that is connected to the BUFRLIB software.
!>
!>  This variable is initialized to a default value which can be overridden by a subsequent call to function isetprm()
!>  within the application program.
  integer :: MAXTBB = 500
end module modv_maxtbb

!> This module declares and initializes the MAXTBD variable.
!>
!> @author J. Ator @date 2014-12-10
module modv_maxtbd
!>  Maximum number of entries in the internal BUFR Table D for each BUFR file that is connected to the BUFRLIB software.
!>
!>  This variable is initialized to a default value which can be overridden by a subsequent call to function isetprm()
!>  within the application program.
  integer :: MAXTBD = 500
end module modv_maxtbd

!> This module declares and initializes the MXBTM variable.
!>
!> @author J. Ator @date 2014-12-10
module modv_mxbtm
!>  Maximum number of bitmaps that can be stored internally for a data subset.
!>
!>  This variable is initialized to a default value which can be overridden by a subsequent call to function isetprm()
!>  within the application program.
  integer :: MXBTM = 5
end module modv_mxbtm

!> This module declares and initializes the MXBTMSE variable.
!>
!> @author J. Ator @date 2014-12-10
module modv_mxbtmse
!>  Maximum number of "set" entries (set to a value of 0) within a bitmap.
!>
!>  This variable is initialized to a default value which can be overridden by a subsequent call to function isetprm()
!>  within the application program.
  integer :: MXBTMSE = 500
end module modv_mxbtmse

!> This module declares and initializes the MXCDV variable.
!>
!> @author J. Ator @date 2014-12-10
module modv_mxcdv
!>  Maximum number of data values that can be written into a data subset of a compressed BUFR message by the
!>  BUFRLIB software.
!>
!>  This variable is initialized to a default value which can be overridden by a subsequent call to function isetprm()
!>  within the application program.
  integer :: MXCDV = 3000
end module modv_mxcdv

!> This module declares and initializes the MXCNEM variable.
!>
!> @author J. Ator @date 2021-03-24
module modv_mxcnem
!>  Maximum number of entries in the internal Table A mnemonic cache that is used for Section 3 decoding of BUFR messages.
  integer, parameter, public :: MXCNEM = 450
end module modv_mxcnem

!> This module declares and initializes the MXCSB variable.
!>
!> @author J. Ator @date 2014-12-10
module modv_mxcsb
!>  Maximum number of data subsets that can be written into a compressed BUFR message by the BUFRLIB software.
!>
!>  This variable is initialized to a default value which can be overridden by a subsequent call to function isetprm()
!>  within the application program.
  integer :: MXCSB = 4000
end module modv_mxcsb

!> This module declares and initializes the MXDXTS variable.
!>
!> @author J. Ator @date 2014-12-10
module modv_mxdxts
!>  Maximum number of dictionary tables that can be stored for use with BUFR messages in internal memory.
!>
!>  This variable is initialized to a default value which can be overridden by a subsequent call to function isetprm()
!>  within the application program.
  integer :: MXDXTS = 200
end module modv_mxdxts

!> This module declares and initializes the MXH4WLC variable.
!>
!> @author J. Ator @date 2014-12-10
module modv_mxh4wlc
!>  Maximum number of long character strings that can be held for writing into an uncompressed BUFR subset by future
!>  internal calls to subroutine writlc().
  integer, parameter, public :: MXH4WLC = 10
end module modv_mxh4wlc

!> This module declares and initializes the MXLCC variable.
!>
!> @author J. Ator @date 2014-12-10
module modv_mxlcc
!>  Maximum length (in bytes) of a character string that can be written into a data subset of a compressed BUFR message
!>  by the BUFRLIB software.
!>
!>  This variable is initialized to a default value which can be overridden by a subsequent call to function isetprm()
!>  within the application program.
  integer :: MXLCC = 32
end module modv_mxlcc

!> This module declares and initializes the MXMSGL variable.
!>
!> @author J. Ator @date 2014-12-10
module modv_mxmsgl
!>  Maximum length (in bytes) of a BUFR message that can be read or written by the BUFRLIB software.
!>
!>  This variable is initialized to a default value which can be overridden by a subsequent call to function isetprm()
!>  within the application program.
  integer :: MXMSGL = 600000
!> The value of mxmsgl divided by 4.
  integer :: MXMSGLD4
end module modv_mxmsgl

!> This module declares and initializes the MXMTBB variable.
!>
!> @author J. Ator @date 2014-12-10
module modv_mxmtbb
!>  Maximum number of entries in a master BUFR Table B.
!>
!>  This variable is initialized to a default value which can be overridden by a subsequent call to function isetprm()
!>  within the application program.
  integer :: MXMTBB = 4000
end module modv_mxmtbb

!> This module declares and initializes the MXMTBD variable.
!>
!> @author J. Ator @date 2014-12-10
module modv_mxmtbd
!>  Maximum number of entries in a master BUFR Table D.
!>
!>  This variable is initialized to a default value which can be overridden by a subsequent call to function isetprm()
!>  within the application program.
  integer :: MXMTBD = 1000
end module modv_mxmtbd

!> This module declares and initializes the MXMTBF variable.
!>
!> @author J. Ator @date 2014-12-10
module modv_mxmtbf
!>  Maximum number of entries in a master BUFR Code/Flag table, counting across all individual Code/Flag tables,
!>  and counting each defined code figure (within each individual Code table) or defined bit number (within each
!>  individual Flag table) as a separate entry.
!>
!>  This variable is initialized to a default value which can be overridden by a subsequent call to function isetprm()
!>  within the application program.
  integer :: MXMTBF = 25000
end module modv_mxmtbf

!> This module declares and initializes the MXNAF variable.
!>
!> @author J. Ator @date 2021-03-24
module modv_mxnaf
!>  Maximum number of associated fields that can be in effect at any given time for a Table B descriptor.
  integer, parameter, public :: MXNAF = 4
end module modv_mxnaf

!> This module declares and initializes the MXNRV variable.
!>
!> @author J. Ator @date 2014-12-10
module modv_mxnrv
!>  Maximum number of entries in the internal jump/link table that can contain new reference values.
!>
!>  This variable is initialized to a default value which can be overridden by a subsequent call to function isetprm()
!>  within the application program.
  integer :: MXNRV = 15
end module modv_mxnrv

!> This module declares and initializes the MXRST variable.
!>
!> @author J. Ator @date 2014-12-10
module modv_mxrst
!>  Maximum number of "long" character strings (greater than 8 bytes) that can be read from a data subset of a compressed
!>  BUFR message.
!>
!>  This variable is initialized to a default value which can be overridden by a subsequent call to function isetprm()
!>  within the application program.
  integer :: MXRST = 50
end module modv_mxrst

!> This module declares and initializes the MXS01V variable.
!>
!> @author J. Ator @date 2014-12-10
module modv_mxs01v
!>  Maximum number of default Section 0 or Section 1 values that can be overwritten within an output BUFR message by
!>  the BUFRLIB software.
!>
!>  This variable is initialized to a default value which can be overridden by a subsequent call to function isetprm()
!>  within the application program.
  integer :: MXS01V = 10
end module modv_mxs01v

!> This module declares and initializes the MXS variable.
!>
!> @author J. Ator @date 2021-03-30
module modv_mxs
!>  Maximum number of entries in the internal string cache.
  integer, parameter, public :: MXS = 1000
end module modv_mxs

!> This module declares and initializes the MXTAMC variable.
!>
!> @author J. Ator @date 2014-12-10
module modv_mxtamc
!>  Maximum number of Table A mnemonics in the internal jump/link table which contain at least one Table C operator
!>  with an XX value of 21 or greater in their definition.
!>
!>  This variable is initialized to a default value which can be overridden by a subsequent call to function isetprm()
!>  within the application program.
  integer :: MXTAMC = 15
end module modv_mxtamc

!> This module declares and initializes the MXTCO variable.
!>
!> @author J. Ator @date 2014-12-10
module modv_mxtco
!>  Maximum number of Table C operators with an XX value of 21 or greater that can appear within the data subset
!>  definition of a Table A mnemonic.
!>
!>  This variable is initialized to a default value which can be overridden by a subsequent call to function isetprm()
!>  within the application program.
  integer :: MXTCO = 30
end module modv_mxtco

!> This module declares and initializes the NFILES variable.
!>
!> @author J. Ator @date 2014-12-10
module modv_nfiles
!>  Maximum number of BUFR files that can be connected to the BUFRLIB software (for reading or writing) at any one time.
!>
!>  This variable is initialized to a default value which can be overridden by a subsequent call to function isetprm()
!>  within the application program.
  integer, public :: NFILES = 32
end module modv_nfiles
