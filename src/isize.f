C> @file
C> @brief Compute the number of characters
C> needed to encode an integer as a string.
C>
C> @author Ator @date 2009-03-23

C> This function computes and returns the number of characters
C> needed to encode the input integer NUM as a string. It does not
C> actually encode the string but rather only figures out the required
C> size. NUM must be an integer in the range of 0 to 99999.
C>
C> @param[in] NUM - integer: number to be encoded
C>
C> @return - integer: number of characters necessary to encode NUM
C> as a string
C>
C> @author Ator @date 2009-03-23
        INTEGER FUNCTION ISIZE (NUM)

        CHARACTER*128 BORT_STR

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

        IF ( NUM .GE. 0 ) THEN
          DO ISIZE = 1, 5
            IF ( NUM .LT. 10**ISIZE ) RETURN
          ENDDO
        ENDIF
        WRITE(BORT_STR,'("BUFRLIB: ISIZE - INPUT NUMBER (",I7,'//
     .    '") IS OUT OF RANGE")') NUM
        CALL BORT(BORT_STR)

        RETURN
        END
