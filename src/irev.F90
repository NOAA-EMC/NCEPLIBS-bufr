!> @file
!> @brief Return a copy of an input integer word with
!> the bytes reversed.
!>
!> @author Woollen @date 1994-01-06

!> If the local machine is "little-endian" (i.e.,
!> if it uses a right to left scheme for numbering the bytes
!> within a machine word), return a copy of an input integer word with
!> the bytes reversed.
!>
!> Although, by definition (within WMO Manual 306),
!> a BUFR message is a stream of individual octets (i.e., bytes)
!> that is independent of any particular machine representation, the
!> NCEPLIBS-bufr software often needs to interpret all or parts
!> of two or more adjacent bytes in order to construct an integer
!> word. By default, the software uses the "big-endian" (left to
!> right) scheme for numbering bytes. By reversing the bytes, irev()
!> allows the integer word to be properly read or written (depending
!> on whether input or output operations, respectively, are being
!> performed) on "little-endian" machines. If the local machine is
!> "big-endian", irev() simply returns a copy of the same integer that was
!> input.
!>
!> @param n - integer word with bytes ordered according to the "big-endian" numbering scheme
!>
!> @return - integer word with bytes ordered according to
!> the numbering scheme of the local machine (either
!> "big-endian" or "little-endian"; if "big-endian" then
!> this is just a direct copy of n).
!>
!> @author Woollen @date 1994-01-06
integer function irev(n) result(iret)

  use modv_vars, only: nbytw, iordle

  implicit none

  integer, intent(in) :: n

  integer int, jnt, i

  character*8 cint,dint

  equivalence(cint,int)
  equivalence(dint,jnt)

#ifdef BIG_ENDIAN
  iret = n
#else
  int = n
  do i=1,nbytw
    dint(i:i) = cint(iordle(i):iordle(i))
  enddo
  iret = jnt
#endif

  return
end function irev
