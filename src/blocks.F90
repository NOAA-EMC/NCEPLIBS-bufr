!> @file
!> @brief Encapsulate a BUFR message with IEEE Fortran control words.
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
!> @param[in,out] mbay - integer(*): BUFR message, possibly with
!>                       added control words on output
!> @param[in,out] mwrd - integer: Size (in integers) of contents
!>                       of mbay
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

!----------------------------------------------------------------------
!----------------------------------------------------------------------

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
