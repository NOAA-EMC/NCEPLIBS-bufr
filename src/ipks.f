C> @file
C> @brief Pack a real*8 value into an integer
C> by applying the proper scale and reference values.
C>
C> @author J. Ator @date 2012-03-02

C> This function packs a real*8 user value into a BUFR
C> integer by applying the proper scale and reference values.
C> Normally the scale and reference values are obtained from index
C> node of the internal jump/link table arrays isc(*) and irf(*);
C> however, the reference value in irf(*) will be overridden if a
C> 2-03 operator is in effect for this node.
C>
C> @param[in] VAL - real*8: user value
C> @param[in] NODE - integer: index into internal jump/link tables
C>
C> @return - integer*8: packed BUFR integer
C>
C> @remarks
C> - This function is the logical inverse of function ups().
C>
C> @author J. Ator @date 2012-03-02
        FUNCTION IPKS(VAL,NODE)

        USE MODA_TABLES
        USE MODA_NRV203

        integer(8) imask, ipks
        REAL*8     TEN,VAL

        DATA TEN /10./

C-----------------------------------------------------------------------

        IPKS = NINT(VAL * TEN**ISC(NODE),8) - IRF(NODE)

        IF ( NNRV .GT. 0 ) THEN

C         There are redefined reference values in the jump/link table,
C         so we need to check if this node is affected by any of them.

          DO JJ = 1, NNRV
            IF ( NODE .EQ. INODNRV(JJ) ) THEN

C             This node contains a redefined reference value.
C             Per the rules of BUFR, negative values should be encoded
C             as positive integers with the left-most bit set to 1.

              NRV(JJ) = NINT(VAL)
              IF ( NRV(JJ) .LT. 0 ) THEN
                IMASK = 2_8**(IBT(NODE)-1)
                IPKS = IOR(ABS(NRV(JJ)),IMASK)
              ELSE
                IPKS = NRV(JJ)
              END IF
              RETURN
            ELSE IF ( ( TAG(NODE)(1:8) .EQ. TAGNRV(JJ) ) .AND.
     .                ( NODE .GE. ISNRV(JJ) ) .AND.
     .                ( NODE .LE. IENRV(JJ) ) ) THEN

C             The corresponding redefinded reference value needs to
C             be used when encoding this value.

              IPKS = NINT(VAL * TEN**ISC(NODE),8) - NRV(JJ)
              RETURN
            END IF
          END DO

        END IF

        RETURN
        END
