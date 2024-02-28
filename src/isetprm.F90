!> @file
!> @brief Define a customized parameter value for dynamic allocation.
!>
!> @author J. Ator @date 2014-12-04

!> Set a specified parameter to a specified value for use in dynamically allocating
!> one or more internal arrays within the NCEPLIBS-bufr software.
!>
!> A separate call to this
!> function must be made for each parameter that is to be set to a
!> customized value, and all such calls must be made prior to the
!> first call to subroutine openbf() from within an application
!> program, because that is when all internal arrays are dynamically
!> allocated based on the parameter values in effect at the time.
!> Otherwise, if this function is never called for a particular
!> parameter, then an internal default value for that parameter is
!> used instead.
!>
!> @param[in] cprmnm - character*(*): Parameter to be changed from its internal default value:
!> - 'MXMSGL' = Maximum length (in bytes) of a BUFR message
!> - 'MAXSS'  = Maximum number of data values in an uncompressed BUFR subset
!> - 'MXCDV'  = Maximum number of data values that can be written into a compressed BUFR subset
!> - 'MXLCC'  = Maximum length (in bytes) of a character string that can  be written into a compressed BUFR subset
!> - 'MXCSB'  = Maximum number of subsets that can be written into a compressed BUFR message
!> - 'NFILES' = Maximum number of BUFR files that can be accessed for reading or writing at any one time
!> - 'MAXTBA' = Maximum number of entries in internal BUFR Table A per BUFR file
!> - 'MAXTBB' = Maximum number of entries in internal BUFR Table B per BUFR file
!> - 'MAXTBD' = Maximum number of entries in internal BUFR Table D per BUFR file
!> - 'MAXMEM' = Maximum number of bytes that can be used to store BUFR messages in internal memory
!> - 'MAXMSG' = Maximum number of BUFR messages that can be stored in internal memory
!> - 'MXDXTS' = Maximum number of dictionary tables that can be stored for use with BUFR messages in internal memory
!> - 'MXMTBB' = Maximum number of master Table B entries
!> - 'MXMTBD' = Maximum number of master Table D entries
!> - 'MXMTBF' = Maximum number of master Code/Flag entries
!> - 'MAXCD'  = Maximum number of child descriptors in a Table D descriptor sequence definition
!> - 'MAXJL'  = Maximum number of entries in the internal jump/link table
!> - 'MXS01V' = Maximum number of default Section 0 or Section 1 values that can be overwritten within an output BUFR message
!> - 'MXBTM'  = Maximum number of bitmaps that can be stored internally for a BUFR subset
!> - 'MXBTMSE' = Maximum number of entries that can be set within a bitmap
!> - 'MXTAMC' = Maximum number of Table A mnemonics in the internal jump/link table which contain at least one Table C operator
!> with XX >= 21 in their subset definition
!> - 'MXTCO'  = Maximum number of Table C operators with XX >= 21 in the subset definition of a Table A mnemonic
!> - 'MXNRV'  = Maximum number of 2-03 reference values in the internal jump/link table
!> - 'MXRST'  = Maximum number of long character strings that can be read from a compressed subset
!> - 'MXH4WLC' = Maximum number of long character strings that can be stored internally
!> - 'MXCNEM' = Maximum number of Table A entries that can be cached during Section 3 decoding of BUFR messages
!> - 'MAXNC' = Maximum number of descriptors within Section 3 of a BUFR message.
!> @param[in] ipval - integer: Value to be set for cprmnm
!> @returns isetprm - integer:
!> -  0 = normal return
!> - -1 = Unknown cprmnm
!>
!> @author J. Ator @date 2014-12-04
recursive function isetprm ( cprmnm, ipval ) result ( iret )

  use modv_vars, only: mxmsgl, maxss, nfiles, mxdxts, maxmsg, maxmem, maxtba, maxtbb, maxtbd, maxjl, &
                       mxcdv, mxlcc, mxcsb, mxmtbb, mxmtbd, mxmtbf, maxcd, mxs01v, mxbtm, mxbtmse, &
                       mxtamc, mxtco, mxnrv, mxrst, mxh4wlc, im8b, mxcnem, maxnc

  implicit none

  character*(*), intent(in) :: cprmnm

  integer, intent(in) :: ipval

  integer iret, my_ipval

  character*128 errstr

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

! Check for I8 integers.

  if ( im8b ) then
    im8b = .false.

    call x84 ( ipval, my_ipval, 1 )
    iret = isetprm ( cprmnm, my_ipval )

    im8b = .true.
    return
  endif

  iret = 0
  if ( cprmnm .eq. 'MAXSS' ) then
    maxss = ipval
  else if ( cprmnm .eq. 'NFILES' ) then
    nfiles = ipval
  else if ( cprmnm .eq. 'MXMSGL' ) then
    mxmsgl = ipval
  else if ( cprmnm .eq. 'MXDXTS' ) then
    mxdxts = ipval
  else if ( cprmnm .eq. 'MAXMSG' ) then
    maxmsg = ipval
  else if ( cprmnm .eq. 'MAXMEM' ) then
    maxmem = ipval
  else if ( cprmnm .eq. 'MAXTBA' ) then
    maxtba = ipval
  else if ( cprmnm .eq. 'MAXTBB' ) then
    maxtbb = ipval
  else if ( cprmnm .eq. 'MAXTBD' ) then
    maxtbd = ipval
  else if ( cprmnm .eq. 'MAXJL' ) then
    maxjl = ipval
  else if ( cprmnm .eq. 'MXCDV' ) then
    mxcdv = ipval
  else if ( cprmnm .eq. 'MXLCC' ) then
    mxlcc = ipval
  else if ( cprmnm .eq. 'MXCSB' ) then
    mxcsb = ipval
  else if ( cprmnm .eq. 'MXMTBB' ) then
    mxmtbb = ipval
  else if ( cprmnm .eq. 'MXMTBD' ) then
    mxmtbd = ipval
  else if ( cprmnm .eq. 'MXMTBF' ) then
    mxmtbf = ipval
  else if ( cprmnm .eq. 'MAXCD' ) then
    maxcd = ipval
  else if ( cprmnm .eq. 'MXS01V' ) then
    mxs01v = ipval
  else if ( cprmnm .eq. 'MXBTM' ) then
    mxbtm = ipval
  else if ( cprmnm .eq. 'MXBTMSE' ) then
    mxbtmse = ipval
  else if ( cprmnm .eq. 'MXTAMC' ) then
    mxtamc = ipval
  else if ( cprmnm .eq. 'MXTCO' ) then
    mxtco = ipval
  else if ( cprmnm .eq. 'MXNRV' ) then
    mxnrv = ipval
  else if ( cprmnm .eq. 'MXRST' ) then
    mxrst = ipval
  else if ( cprmnm .eq. 'MXH4WLC' ) then
    mxh4wlc = ipval
  else if ( cprmnm .eq. 'MXCNEM' ) then
    mxcnem = ipval
  else if ( cprmnm .eq. 'MAXNC' ) then
    maxnc = ipval
  else
    iret = -1
    call errwrt('++++++++++++++++++WARNING+++++++++++++++++++')
    errstr = 'BUFRLIB: ISETPRM - UNKNOWN INPUT PARAMETER '// CPRMNM // ' -- NO ACTION WAS TAKEN'
    call errwrt(errstr)
    call errwrt('++++++++++++++++++WARNING+++++++++++++++++++')
  endif

  return
end function isetprm
