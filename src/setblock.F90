!> @file
!> @brief Specify the use of IEEE Fortran control words when writing
!> BUFR messages.
!>
!> @author J. Woollen @date 2012-09-15

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
!> @param[in] iblk - integer: Flag indicating whether future BUFR
!>                   output messages should be encapsulated with
!>                   control words
!>                      - -1 = Yes, using little-endian control words
!>                      -  0 = No (the default)
!>                      -  1 = Yes, using big-endian control words
!>
!> @author J. Woollen @date 2012-09-15
recursive subroutine setblock(iblk)

  use modv_vars, only: im8b, iblock

  implicit none

  integer, intent(in) :: iblk

  integer my_iblk

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

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
