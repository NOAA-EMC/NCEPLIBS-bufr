C> @file
C> @author WOOLLEN @date 1994-01-06
      
C> THIS SUBROUTINE UNPACKS AND RETURNS AN 8-BYTE INTEGER
C>   CONTAINED WITHIN NBITS BITS OF IBAY, STARTING WITH BIT (IBIT+1).
C>   ON OUTPUT, IBIT IS UPDATED TO POINT TO THE LAST BIT THAT WAS
C>   UNPACKED.  THIS IS SIMILAR TO BUFR ARCHIVE LIBRARY SUBROUTINE UPB8,
C>   EXCEPT IN UPB8 IBIT IS NOT UPDATED UPON OUTPUT (AND THE ORDER OF
C>   ARGUMENTS IS DIFFERENT).
C>
C> PROGRAM HISTORY LOG:
C> 2022-05-06  J. WOOLLEN -- ORIGINAL AUTHOR
C>
C> USAGE:    CALL UP8 (NVAL, NBITS, IBAY, IBIT)
C>   INPUT ARGUMENT LIST:
C>     NBITS    - INTEGER: NUMBER OF BITS OF IBAY WITHIN WHICH TO UNPACK
C>                NVAL
C>     IBAY     - INTEGER: *-WORD PACKED BINARY ARRAY CONTAINING PACKED
C>                NVAL
C>     IBIT     - INTEGER: BIT POINTER WITHIN IBAY INDICATING BIT AFTER
C>                WHICH TO START UNPACKING
C>
C>   OUTPUT ARGUMENT LIST:
C>     NVAL     - INTEGER*8: UNPACKED INTEGER
C>     IBIT     - INTEGER: BIT POINTER WITHIN IBAY INDICATING LAST BIT
C>                THAT WAS UNPACKED
C>
C> REMARKS:
C>    THIS SUBROUTINE IS THE INVERSE OF BUFR ARCHIVE LIBRARY ROUTINE
C>    PKB8.
C>
C>    THIS ROUTINE CALLS:        UPB8
C>    THIS ROUTINE IS CALLED BY: RDCMPS   WRCMPS   UFBTAB   
C>                               Normally not called by any application
C>                               programs.
C>
!----------------------------------------------------------------------
!----------------------------------------------------------------------

      subroutine up8(nval,nbits,ibay,ibit)

      common /hrdwrd/ nbytw,nbitw,iord(8)

      integer(8) :: nval
      integer(4) :: nbits,ibit,ibay(*)

      call upb8(nval,nbits,ibit,ibay)
      ibit = ibit+nbits

      end subroutine
