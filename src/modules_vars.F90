!> @file
!> @brief Declare and initialize module variables.
!>
!> @author J. Ator @date 2023-02-10

module modv_vars

  !> Number of bytes within an integer.
  integer, parameter :: nbytw = 4

  !> Order of bytes within an integer on a big-endian machine, from most to least significant.
  integer, parameter :: iordbe(nbytw) = (/1,2,3,4/)

  !> Order of bytes within an integer on a little-endian machine, from most to least significant.
  integer, parameter :: iordle(nbytw) = (/4,3,2,1/)

  !> Number of bits within an integer.
  integer, parameter :: nbitw = nbytw * 8

  !> Status indicator to keep track of whether all future calls to
  !> NCEPLIBS-bufr subroutines and functions from a Fortran application
  !> program will be made using 8-byte integer arguments.
  !> The default value is .false., meaning that all future calls to
  !> NCEPLIBS-bufr subroutines and functions will be made using 4-byte
  !> integer arguments.  This value can be changed at any time via a
  !> call to subroutine setim8b().
  logical :: im8b = .false.

  !> Status indicator to keep track of whether all future BUFR output
  !> messages should be encapsulated with IEEE Fortran control words:
  !> - -1 = Yes, using little-endian control words
  !> - 0 = No
  !> - 1 = Yes, using big-endian control words
  !> The default value is 0, but this value can be changed at any
  !> time via a call to subroutine setblock().
  integer :: iblock = 0

  !> Current placeholder value to represent "missing" data when reading
  !> from or writing to BUFR files; this value can be changed at any
  !> time via a call to subroutine setbmiss().
  real*8 :: bmiss = 10E10_8

  !> Status indicator to keep track of whether subroutine openbf() has already been called:
  !> - 0 = No
  !> - 1 = Yes
  integer :: ifopbf = 0

  !> Status indicator to keep track of whether future calls to subroutine parusr() should
  !> allow an input mnemonic to exist in multiple replication sequences:
  !> - 0 = No
  !> - 1 = Yes
  integer :: iac = 0

  !> Length of Section 1 date-time values to be output by all future calls to message-reading
  !> subroutines.  The default value is 8, meaning that future date-time values will be
  !> output in YYMMDDHH (2-digit year) format.  However, this value can be changed to 10 via
  !> a call to subroutine datelen(), and in which case future date-time values will be output
  !> in YYYYMMDDHH (4-digit year) format.
  integer :: lendat = 8

  !> Replication indicators used in DX BUFR tables.
  character, parameter :: reps(10) =    (/     '"',     '(',     '{',     '[',     '<', &
                                               '"',     ')',     '}',     ']',     '>'/)

  !> Replication tags corresponding to reps.
  character*3, parameter :: typs(10) =  (/   'REP',   'DRP',   'DRP',   'DRS',   'DRB', &
                                             'SEQ',   'RPC',   'RPC',   'RPS',   'SEQ'/)

  !> FXY values corresponding to reps.
  character*6, parameter :: adsn(10) =  (/'101000','360001','360002','360003','360004', &
                                          '101255','031002','031001','031001','031000'/)

  !> WMO bit-wise representations of FXY values corresponding to reps.
  integer :: idnr(10)

  !> Lengths of delayed replication factors corresponding to each type of replication in reps.
  integer, parameter :: lens(5) =       (/       0,      16,       8,       8,       1/)

  !> Maximum number of child descriptors that can be included within
  !> the sequence definition of a Table D descriptor, not counting the
  !> recursive resolution of any child descriptors which may themselves
  !> be Table D descriptors.
  !> This variable is initialized to a default value which can be
  !> overridden by a subsequent call to function isetprm() within the
  !> application program.
  integer :: maxcd = 250

  !> Maximum number of entries in the internal jump/link table.
  !> This variable is initialized to a default value which can be
  !> overridden by a subsequent call to function isetprm() within the
  !> application program.
  integer :: maxjl = 96000

  !> Maximum number of bytes that can be used to store BUFR messages
  !> within internal memory.
  !> This variable is initialized to a default value which can be
  !> overridden by a subsequent call to function isetprm() within the
  !> application program.
  integer :: maxmem = 50000000

  !> Maximum number of BUFR messages that can be stored within internal
  !> memory.
  !> This variable is initialized to a default value which can be
  !> overridden by a subsequent call to function isetprm() within the
  !> application program.
  integer :: maxmsg = 200000

  !> Maximum number of descriptors within Section 3 of a BUFR message.
  !> This variable is initialized to a default value which can be
  !> overridden by a subsequent call to function isetprm() within the
  !> application program.
  integer :: maxnc = 600

  !> Maximum number of data values that can be read from or written
  !> into a data subset by the NCEPLIBS-bufr software.
  !> This variable is initialized to a default value which can be
  !> overridden by a subsequent call to function isetprm() within the
  !> application program.
  integer :: maxss = 120000

  !> Maximum number of entries in the internal BUFR Table A for each
  !> BUFR file that is connected to the NCEPLIBS-bufr software.
  !> This variable is initialized to a default value which can be
  !> overridden by a subsequent call to function isetprm() within the
  !> application program.
  integer :: maxtba = 150

  !> Maximum number of entries in the internal BUFR Table B for each
  !> BUFR file that is connected to the NCEPLIBS-bufr software.
  !> This variable is initialized to a default value which can be
  !> overridden by a subsequent call to function isetprm() within the
  !> application program.
  integer :: maxtbb = 500

  !> Maximum number of entries in the internal BUFR Table D for each
  !> BUFR file that is connected to the NCEPLIBS-bufr software.
  !> This variable is initialized to a default value which can be
  !> overridden by a subsequent call to function isetprm() within the
  !> application program.
  integer :: maxtbd = 500

  !> Maximum number of bitmaps that can be stored internally for a data subset.
  !> This variable is initialized to a default value which can be
  !> overridden by a subsequent call to function isetprm() within the
  !> application program.
  integer :: mxbtm = 5

  !> Maximum number of "set" entries (set to a value of 0) within a bitmap.
  !> This variable is initialized to a default value which can be
  !> overridden by a subsequent call to function isetprm() within the
  !> application program.
  integer :: mxbtmse = 500

  !> Maximum number of data values that can be written into a data
  !> subset of a compressed BUFR message by the NCEPLIBS-bufr software.
  !> This variable is initialized to a default value which can be
  !> overridden by a subsequent call to function isetprm() within the
  !> application program.
  integer :: mxcdv = 3000

  !> Maximum number of entries in the internal Table A mnemonic cache
  !> that is used for Section 3 decoding of BUFR messages.
  !> This variable is initialized to a default value which can be
  !> overridden by a subsequent call to function isetprm() within the
  !> application program.
  integer :: mxcnem = 450

  !> Maximum number of data subsets that can be written into a
  !> compressed BUFR message by the NCEPLIBS-bufr software.
  !> This variable is initialized to a default value which can be
  !> overridden by a subsequent call to function isetprm() within the
  !> application program.
  integer :: mxcsb = 4000

  !> Maximum number of dictionary tables that can be stored for use
  !> with BUFR messages in internal memory.
  !> This variable is initialized to a default value which can be
  !> overridden by a subsequent call to function isetprm() within the
  !> application program.
  integer :: mxdxts = 200

  !> Maximum number of long character strings that can be held for
  !> writing into an uncompressed BUFR subset by future internal calls
  !> to subroutine writlc().
  !> This variable is initialized to a default value which can be
  !> overridden by a subsequent call to function isetprm() within the
  !> application program.
  integer :: mxh4wlc = 10

  !> Maximum length (in bytes) of a character string that can be
  !> written into a data subset of a compressed BUFR message
  !> by the NCEPLIBS-bufr software.
  !> This variable is initialized to a default value which can be
  !> overridden by a subsequent call to function isetprm() within the
  !> application program.
  integer :: mxlcc = 32

  !> Maximum length (in bytes) of a BUFR message that can be read or
  !> written by the NCEPLIBS-bufr software.
  !> This variable is initialized to a default value which can be
  !> overridden by a subsequent call to function isetprm() within the
  !> application program.
  integer :: mxmsgl = 600000

  !> The value of mxmsgl divided by 4.
  integer :: mxmsgld4

  !> Maximum number of entries in a master BUFR Table B.
  !> This variable is initialized to a default value which can be
  !> overridden by a subsequent call to function isetprm() within the
  !> application program.
  integer :: mxmtbb = 4000

  !> Maximum number of entries in a master BUFR Table D.
  !> This variable is initialized to a default value which can be
  !> overridden by a subsequent call to function isetprm() within the
  !> application program.
  integer :: mxmtbd = 1000

  !> Maximum number of entries in a master BUFR Code/Flag table,
  !> counting across all individual Code/Flag tables, and counting each
  !> defined code figure (within each individual Code table) or defined
  !> bit number (within each individual Flag table) as a separate entry.
  !> This variable is initialized to a default value which can be
  !> overridden by a subsequent call to function isetprm() within the
  !> application program.
  integer :: mxmtbf = 25000

  !> Maximum number of associated fields that can be in effect at any
  !> given time for a Table B descriptor.
  integer :: mxnaf = 4

  !> Maximum number of entries in the internal jump/link table that can
  !> contain new reference values.
  !> This variable is initialized to a default value which can be
  !> overridden by a subsequent call to function isetprm() within the
  !> application program.
  integer :: mxnrv = 15

  !> Maximum number of "long" character strings (greater than 8 bytes)
  !> that can be read from a data subset of a compressed BUFR message.
  !> This variable is initialized to a default value which can be
  !> overridden by a subsequent call to function isetprm() within the
  !> application program.
  integer :: mxrst = 50

  !> Maximum number of default Section 0 or Section 1 values that can
  !> be overwritten within an output BUFR message by the NCEPLIBS-bufr
  !> software.
  !> This variable is initialized to a default value which can be
  !> overridden by a subsequent call to function isetprm() within the
  !> application program.
  integer :: mxs01v = 10

  !> Maximum number of entries in the internal string cache.
  integer, parameter :: mxs = 1000

  !> Maximum number of recursion levels when expanding a subset template.
  integer, parameter :: maxrcr = 100

  !> Maximum number of Table A mnemonics in the internal jump/link
  !> table which contain at least one Table C operator with an XX value
  !> of 21 or greater in their definition.
  !> This variable is initialized to a default value which can be
  !> overridden by a subsequent call to function isetprm() within the
  !> application program.
  integer :: mxtamc = 15

  !> Maximum number of Table C operators with an XX value of 21 or
  !> greater that can appear within the data subset definition of a
  !> Table A mnemonic.
  !> This variable is initialized to a default value which can be
  !> overridden by a subsequent call to function isetprm() within the
  !> application program.
  integer :: mxtco = 30

  !> Maximum number of BUFR files that can be connected to the NCEPLIBS-bufr
  !> software (for reading or writing) at any one time.
  !> This variable is initialized to a default value which can be
  !> overridden by a subsequent call to function isetprm() within the
  !> application program.
  integer :: nfiles = 32

end module modv_vars
