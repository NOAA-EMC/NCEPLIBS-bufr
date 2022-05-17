C> @file
C> @author WOOLLEN @date 1994-01-06
      
C> THIS SUBROUTINE UNPACKS AND RETURNS AN 8byte INTEGER
C>   CONTAINED WITHIN NBITS BITS OF IBAY, STARTING WITH BIT (IBIT+1).
C>   THIS IS SIMILAR TO BUFR ARCHIVE LIBRARY SUBROUTINE UP8, EXCEPT IN
C>   UPBB IBIT IS NOT UPDATED UPON OUTPUT (AND THE ORDER OF ARGUMENTS IS
C>   DIFFERENT).
C>
C> PROGRAM HISTORY LOG:
C> 2022-05-06  J. WOOLLEN -- ORIGINAL AUTHOR
C>
C> USAGE:    CALL UPBB (NVAL, NBITS, IBIT, IBAY)
C>   INPUT ARGUMENT LIST:
C>     NBITS    - INTEGER: NUMBER OF BITS OF IBAY WITHIN WHICH TO UNPACK
C>                NVAL
C>     IBIT     - INTEGER: BIT POINTER WITHIN IBAY TO START UNPACKING
C>                FROM
C>     IBAY     - INTEGER: *-WORD PACKED BINARY ARRAY CONTAINING PACKED
C>                NVAL
C>
C>   OUTPUT ARGUMENT LIST:
C>     NVAL     - 8byte INTEGER: UNPACKED INTEGER
C>
C> REMARKS:
C>    THIS ROUTINE CALLS:        UPB      UPBB
C>    THIS ROUTINE IS CALLED BY: RCSTPL   RDTREE   UFBGET   UFBTAB
C>                               UFBTAM                         
C>                               Normally not called by any application
C>                               programs.
C>
!----------------------------------------------------------------------
!----------------------------------------------------------------------

      subroutine upb8(nval,nbits,ibit,ibay)

      common /hrdwrd/ nbytw,nbitw,iord(8)

      integer(8) :: nval
      integer(4) :: nbits,ibit,ibay(*)

      integer(4) :: nvals(2)
      integer(8) :: nval8
      equivalence (nval8,nvals)

      if(nbits<0 ) call bort('BUFRLIB: UPB8 - nbits < zero !!!!!')
      if(nbits>64) nval=0
      if(nbits>64) return

      if(nbitw==32) then
         jbit=ibit; nvals=0
         call upb(nvals(2),max(nbits-nbitw,0),ibay,jbit)
         call upb(nvals(1),min(nbitw,nbits  ),ibay,jbit)
         nval=nval8
      else
         call upbb(nval,nbits,ibit,ibay)
      endif

      end subroutine

