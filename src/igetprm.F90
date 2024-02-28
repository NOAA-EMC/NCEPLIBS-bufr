!> @file
!> @brief Get the current value of a parameter.
!>
!> @author J. Ator @date 2014-12-04

!> Return the current value of a parameter used for allocating one or more internal arrays within the
!> NCEPLIBS-bufr software.
!>
!> @param[in] cprmnm - character*(*): Parameter
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
!>
!> @returns igetprm - integer: Value of cprmnm:
!> - -1 Unknown cprmnm
!>
!> @author J. Ator @date 2014-12-04
integer function igetprm ( cprmnm ) result ( iret )

  use modv_vars, only: mxmsgl, maxss, nfiles, mxdxts, maxmsg, maxmem, maxtba, maxtbb, maxtbd, maxjl, &
                       mxcdv, mxlcc, mxcsb, mxmtbb, mxmtbd, mxmtbf, maxcd, mxs01v, mxbtm, mxbtmse, &
                       mxtamc, mxtco, mxnrv, mxrst, mxh4wlc, mxcnem, maxnc

  implicit none

  character*(*), intent(in) :: cprmnm

  character*64 errstr

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

  if ( cprmnm .eq. 'MAXSS' ) then
    iret = maxss
  else if ( cprmnm .eq. 'NFILES' ) then
    iret = nfiles
  else if ( cprmnm .eq. 'MXMSGL' ) then
    iret = mxmsgl
  else if ( cprmnm .eq. 'MXDXTS' ) then
    iret = mxdxts
  else if ( cprmnm .eq. 'MAXMSG' ) then
    iret = maxmsg
  else if ( cprmnm .eq. 'MAXMEM' ) then
    iret = maxmem
  else if ( cprmnm .eq. 'MAXTBA' ) then
    iret = maxtba
  else if ( cprmnm .eq. 'MAXTBB' ) then
    iret = maxtbb
  else if ( cprmnm .eq. 'MAXTBD' ) then
    iret = maxtbd
  else if ( cprmnm .eq. 'MAXJL' ) then
    iret = maxjl
  else if ( cprmnm .eq. 'MXCDV' ) then
    iret = mxcdv
  else if ( cprmnm .eq. 'MXLCC' ) then
    iret = mxlcc
  else if ( cprmnm .eq. 'MXCSB' ) then
    iret = mxcsb
  else if ( cprmnm .eq. 'MXMTBB' ) then
    iret = mxmtbb
  else if ( cprmnm .eq. 'MXMTBD' ) then
    iret = mxmtbd
  else if ( cprmnm .eq. 'MXMTBF' ) then
    iret = mxmtbf
  else if ( cprmnm .eq. 'MAXCD' ) then
    iret = maxcd
  else if ( cprmnm .eq. 'MXS01V' ) then
    iret = mxs01v
  else if ( cprmnm .eq. 'MXBTM' ) then
    iret = mxbtm
  else if ( cprmnm .eq. 'MXBTMSE' ) then
    iret = mxbtmse
  else if ( cprmnm .eq. 'MXTAMC' ) then
    iret = mxtamc
  else if ( cprmnm .eq. 'MXTCO' ) then
    iret = mxtco
  else if ( cprmnm .eq. 'MXNRV' ) then
    iret = mxnrv
  else if ( cprmnm .eq. 'MXRST' ) then
    iret = mxrst
  else if ( cprmnm .eq. 'MXH4WLC' ) then
    iret = mxh4wlc
  else if ( cprmnm .eq. 'MXCNEM' ) then
    iret = mxcnem
  else if ( cprmnm .eq. 'MAXNC' ) then
    iret = maxnc
  else
    iret = -1
    call errwrt('++++++++++++++++++WARNING+++++++++++++++++++')
    errstr = 'BUFRLIB: IGETPRM - UNKNOWN INPUT PARAMETER '// CPRMNM
    call errwrt(errstr)
    call errwrt('++++++++++++++++++WARNING+++++++++++++++++++')
  endif

  return
end function igetprm
