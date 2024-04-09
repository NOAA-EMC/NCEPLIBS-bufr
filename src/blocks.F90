!> @file
!> @brief Block BUFR messages with IEEE Fortran control words
!> @author J. Woollen @date 2012-09-15

!> Encapsulate a BUFR message with IEEE Fortran
!> control words as specified via the most recent call to
!> subroutine setblock().
!>
!> A previous call to subroutine setblock() is required in
!> order to activate encapsulation with control words, and to
!> specify whether the control words should be encoded using
!> big-endian or little-endian byte ordering.  In such cases,
!> the input parameter mbay is then modified to
!> add the specified control words to the existing BUFR message
!> whenever this subroutine is called, and mwrd is also
!> modified accordingly.
!>
!> Alternatively, if subroutine setblock() was never previously
!> called, or if no encapsulation was specified during the most
!> recent call to subroutine setblock(), then this subroutine
!> simply returns without modifying either of its input parameters.
!>
!> @param[in,out] mbay - BUFR message, possibly with added control words on output
!> @param[in,out] mwrd - Size (in integers) of contents of mbay
!>
!> @remarks
!> - For more information about IEEE Fortran control words, as
!> well as their historical use within the NCEPLIBS-bufr software, see
!> the documentation for subroutine setblock().
!> - Whenever a BUFR message in mbay is to be encapsulated with
!> control words, the user must ensure the availability of
!> sufficient extra space when allocating mbay within the
!> application program.
!>
!> @author J. Woollen @date 2012-09-15
subroutine blocks(mbay,mwrd)

  use modv_vars, only: iblock, nbytw, iordle

  implicit none

  integer, intent(inout) :: mbay(*), mwrd

  integer iint, jint, i

  character*1 cint(4),dint(4)

  equivalence(cint,iint)
  equivalence(dint,jint)

  if(iblock.eq.0) return

  ! make room in mbay for control words - one at each end of the record

  do i=mwrd,1,-1
    mbay(i+1) = mbay(i)
  enddo

  ! store the endianized control word in bytes in dint/jint

  iint=mwrd*4

  do i=1,nbytw
    if(iblock.eq.-1) then
#ifdef BIG_ENDIAN
      dint(i)=cint(iordle(i))
#else
      dint(i)=cint(i)
#endif
    elseif(iblock.eq.1) then
#ifdef LITTLE_ENDIAN
      dint(i)=cint(iordle(i))
#else
      dint(i)=cint(i)
#endif
    endif
  enddo

  ! increment mrwd and install the control words in their proper places

  mwrd = mwrd+2
  mbay(1) = jint
  mbay(mwrd) = jint

  return
end subroutine blocks

!> Specify whether BUFR messages output by
!> future calls to [message-writing subroutines](@ref hierarchy)
!> should be encapsulated with IEEE Fortran control words when being
!> written to output files.
!>
!> If control words are requested, then one 4-byte control word is
!> written to the output file prior to the start of each BUFR message,
!> and a second 4-byte control word is written to the output file after
!> the end of each BUFR message. Each of these control words contains
!> the byte count for the enclosed BUFR message, and they can be
!> written using either big-endian or little-endian byte ordering,
!> regardless of the native endianness of the local machine.
!>
!> This subroutine can be called at any time after the first call
!> to subroutine openbf(), and the specified value for iblk will remain
!> in effect for all future calls to
!> [message-writing subroutines](@ref hierarchy) for all Fortran logical
!> units that are open for output within the application program,
!> unless a subsequent call is made to this subroutine to reset the
!> value of iblk again. If this subroutine is never called, a default
!> value of 0 is used for iblk, as set within subroutine bfrini().
!>
!> @remarks
!> - This subroutine can be used to generate BUFR files consistent
!> with historical archives, dating back to older versions of the
!> NCEPLIBS-bufr software which used Fortran to directly read/write
!> BUFR messages from/to files. Standard Fortran historically
!> didn't have a way to read/write binary data streams without
!> control words, so as a result many historical archives contain
!> these by default. However, newer versions of the NCEPLIBS-bufr software
!> use C to directly read/write BUFR messages from/to files
!> (including historical archives), so control words are no longer
!> necessary and are therefore now disabled by default when writing
!> BUFR messages to output files.
!>
!> @param[in] iblk - Flag indicating whether future BUFR output messages should be encapsulated with control words:
!>    - -1 = Yes, using little-endian control words
!>    -  0 = No (the default)
!>    -  1 = Yes, using big-endian control words
!>
!> @author J. Woollen @date 2012-09-15
recursive subroutine setblock(iblk)

  use modv_vars, only: im8b, iblock

  implicit none

  integer, intent(in) :: iblk

  integer my_iblk

  ! Check for I8 integers.

  if(im8b) then
    im8b=.false.

    call x84(iblk,my_iblk,1)
    call setblock(my_iblk)

    im8b=.true.
    return
  endif

  iblock=iblk

  return
end subroutine setblock
