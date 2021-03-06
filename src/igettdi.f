C> @file
C> @author ATOR @date 2009-03-23
	
C> DEPENDING ON THE VALUE OF THE INPUT FLAG, THIS FUNCTION
C>   EITHER RETURNS THE NEXT USABLE SCRATCH TABLE D INDEX FOR THE
C>   CURRENT MASTER TABLE OR ELSE RESETS THE INDEX BACK TO ITS
C>   MINIMUM VALUE.
C>
C> PROGRAM HISTORY LOG:
C> 2009-03-23  J. ATOR    -- ORIGINAL AUTHOR
C>
C> USAGE:    IGETTDI ( IFLAG )
C>   INPUT ARGUMENT LIST:
C>     IFLAG    - INTEGER: FLAG: IF SET TO 0, THEN THE FUNCTION WILL
C>                RESET THE SCRATCH TABLE D INDEX BACK TO ITS MINIMUM
C>                VALUE; OTHERWISE, IT WILL RETURN THE NEXT USABLE
C>                SCRATCH TABLE D INDEX FOR THE CURRENT MASTER TABLE
C>
C>   OUTPUT ARGUMENT LIST:
C>     IGETTDI  - INTEGER: NEXT USABLE SCRATCH TABLE D INDEX FOR THE
C>                CURRENT MASTER TABLE
C>                  -1 = FUNCTION WAS CALLED WITH IFLAG=0
C>
C> REMARKS:
C>    THIS ROUTINE CALLS:        BORT
C>    THIS ROUTINE IS CALLED BY: READS3   STSEQ
C>                               Not normally called by application
C>                               programs.
C>
	FUNCTION IGETTDI ( IFLAG )



	PARAMETER ( IDXMIN = 62976 )
C*			   = IFXY('354000')

	PARAMETER ( IDXMAX = 63231 )
C*			   = IFXY('354255')

	CHARACTER*128	BORT_STR

	SAVE IDX

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

	IF ( IFLAG .EQ. 0 ) THEN

C*	  Initialize the index to one less than the actual minimum
C*	  value.  That way, the next normal call will return the
C*	  minimum value.

	  IDX = IDXMIN - 1
	  IGETTDI = -1
	ELSE
	  IDX = IDX + 1
	  IF ( IDX .GT. IDXMAX ) GOTO 900
	  IGETTDI = IDX
	ENDIF

	RETURN
 900	CALL BORT('BUFRLIB: IGETTDI - IDXMAX OVERFLOW')
	END
