C> @file
C> @brief Pack a real*8 value into an integer
C> by applying the proper scale and reference values.
C>
C> ### Program History Log
C> Date | Programmer | Comments
C> -----|------------|----------
C> 2012-03-02 | J. Ator    | Original author; adapted from internal statement function in wrtree.
C> 2014-12-10 | J. Ator    | Use modules instead of common blocks.
C> 2022-05-06 | J. Woollen | Make imask and ipks 8byte integers.
C>
C> @author J. Ator @date 2012-03-02

C> This function packs a real*8 user value into an
C> integer by applying the proper scale and reference values.
C> Normally the scale and reference values are obtained from index
C> node of the internal jump/link table arrays isc(*) and irf(*);
C> however, the reference value in irf(*) will be overridden if a
C> 2-03 operator is in effect for this node.
C>
C> @param[in] VAL - real*8: user value
C> @param[in] NODE - integer: index into internal jump/link tables
C>
C> @return - integer*8: packed value
C>
C> @author J. Ator @date 2012-03-02
	FUNCTION IPKS(VAL,NODE)

	USE MODA_TABLES
	USE MODA_NRV203

        integer(8) imask, ipks
	REAL*8	   TEN,VAL

	DATA TEN /10./

C-----------------------------------------------------------------------

	IPKS = VAL * TEN**ISC(NODE) - IRF(NODE) + .5

	IF ( NNRV .GT. 0 ) THEN

C	  There are redefined reference values in the jump/link table,
C	  so we need to check if this node is affected by any of them.

	  DO JJ = 1, NNRV
	    IF ( NODE .EQ. INODNRV(JJ) ) THEN

C	      This node contains a redefined reference value.
C	      Per the rules of BUFR, negative values should be encoded
C	      as positive integers with the left-most bit set to 1.

	      NRV(JJ) = NINT(VAL)
	      IF ( NRV(JJ) .LT. 0 ) THEN
		IMASK = 2_8**(IBT(NODE)-1)
		IPKS = IOR(ABS(NRV(JJ)),IMASK)
	      ELSE
		IPKS = NRV(JJ)
	      END IF
	      RETURN
	    ELSE IF ( ( TAG(NODE)(1:8) .EQ. TAGNRV(JJ) ) .AND.
     .		      ( NODE .GE. ISNRV(JJ) ) .AND.
     .		      ( NODE .LE. IENRV(JJ) ) ) THEN

C	      The corresponding redefinded reference value needs to
C	      be used when encoding this value.

	      IPKS = VAL * TEN**ISC(NODE) - NRV(JJ) + .5
	      RETURN
	    END IF
	  END DO

	END IF

	RETURN
	END
