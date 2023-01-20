C> @file
C> @brief Returns the next usable scratch table D index for the
C> current master table or else resets the index back to its
C> minimum value.	
C> @author Ator @date 2009-03-23
	
C> Depending on the value of the input flag, this function
C> either returns the next usable scratch table D index for the
C> current master table or else resets the index back to its
C> minimum value.
C>
C> @param IFLAG - integer: flag: if set to 0, then the function will
C>                reset the scratch table d index back to its minimum
C>                value; otherwise, it will return the next usable
C>                scratch table d index for the current master table.
C>
C> @return - integer: next usable scratch table d index for the
C> current master table, or -1 if function was called with iflag=0.
C>
C> @author Ator @date 2009-03-23
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
