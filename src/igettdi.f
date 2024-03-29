C> @file
C> @brief Get the next usable Table D index for the
C> current master table, or reset the index.
C>
C> @author Ator @date 2009-03-23

C> Depending on the value of the input flag, 
C> either return the next usable scratch Table D index for the
C> current master table, or else reset the index back to its
C> minimum value.
C>
C> @param[in] IFLAG - integer:
C>   - if set to 0, then the function will reset the scratch Table D index
C>     back to its minimum value
C>   - otherwise, the function will return the next usable scratch Table D
C>     index for the current master table
C>
C> @return - integer:
C>   - -1 if function was called with IFLAG=0
C>   - otherwise, the next usable scratch Table D index for the
C>     current master table
C>
C> @author J. Ator @date 2009-03-23
        FUNCTION IGETTDI ( IFLAG )

        PARAMETER ( IDXMIN = 62976 )
C*                         = IFXY('354000')

        PARAMETER ( IDXMAX = 63231 )
C*                         = IFXY('354255')

        SAVE IDX

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

        IF ( IFLAG .EQ. 0 ) THEN

C*        Initialize the index to one less than the actual minimum
C*        value.  That way, the next normal call will return the
C*        minimum value.

          IDX = IDXMIN - 1
          IGETTDI = -1
        ELSE
          IDX = IDX + 1
          IF ( IDX .GT. IDXMAX ) GOTO 900
          IGETTDI = IDX
        ENDIF

        RETURN
 900    CALL BORT('BUFRLIB: IGETTDI - IDXMAX OVERFLOW')
        END
