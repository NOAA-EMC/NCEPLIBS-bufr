C> @file
C> $brief unpacks a real*8 user value from a packed bufr integer
C>
C> ### Program History Log
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> 2012-03-02 | j. ator  |  original author; adapted from internal
C>                           statement function in other subroutines
C> 2014-12-10 | j. ator  |  use modules instead of common blocks
C> 2022-05-06 | j. woollen  |  make ival and imask 8byte integers
C>
C> @author J. Ator @date 2012-03-02

C> this function unpacks a real*8 user value from a packed
C> bufr integer by applying the proper scale and reference values.
C> normally the scale and reference values are obtained from index
C> node of the internal jump/link table arrays isc(*) and irf(*);
C> however, the reference value in irf(*) will be overridden if a
C> 2-03 operator is in effect for this node.
C>
C> @param[in] ival - integer: packed bufr integer
C> @param[in] node - integer: index into internal jump/link tables
C> @param[out] ups - real*8: user value
C>
C> @author J. Ator @date 2012-03-02

	REAL*8 FUNCTION UPS(IVAL,NODE)

	USE MODA_TABLES
	USE MODA_NRV203

        integer*8 ival,imask
	REAL*8	  TEN

	DATA TEN /10./

C-----------------------------------------------------------------------

	UPS = ( IVAL + IRF(NODE) ) * TEN**(-ISC(NODE))

	IF ( NNRV .GT. 0 ) THEN

C	  There are redefined reference values in the jump/link table,
C	  so we need to check if this node is affected by any of them.

	  DO JJ = 1, NNRV
	    IF ( NODE .EQ. INODNRV(JJ) ) THEN

C	      This node contains a redefined reference value.
C	      Per the rules of BUFR, negative values may be encoded
C	      as positive integers with the left-most bit set to 1.

	      IMASK = 2_8**(IBT(NODE)-1)
	      IF ( IAND(IVAL,IMASK) .GT. 0 ) THEN
		NRV(JJ) = (-1) * ( IVAL - IMASK )
	      ELSE
		NRV(JJ) = IVAL
	      END IF
	      UPS = NRV(JJ)
	      RETURN
	    ELSE IF ( ( TAG(NODE)(1:8) .EQ. TAGNRV(JJ) ) .AND.	    
     .		      ( NODE .GE. ISNRV(JJ) ) .AND.
     .		      ( NODE .LE. IENRV(JJ) ) ) THEN

C	      The corresponding redefinded reference value needs to
C	      be used when decoding this value.

	      UPS = ( IVAL + NRV(JJ) ) * TEN**(-ISC(NODE))
	      RETURN
	    END IF
	  END DO

	END IF

	RETURN
	END
