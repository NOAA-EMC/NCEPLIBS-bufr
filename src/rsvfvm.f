C> @file
C> @brief Process a "following value" mnemonic.
C>
C> ### Program History Log
C> Date | Programmer | Comments
C> -----|------------|----------
C> 1994-01-06 | J. Woollen | original author
C> 2003-11-04 | J. Ator    | added documentation
C> 2003-11-04 | S. Bender  | added remarks/bufrlib routine interdependencies
C> 2003-11-04 | D. Keyser  | unified/portable for wrf; added history documentation
C>
C> @author Woollen @date 1994-01-06

C> This subroutine steps through the "following value"
C> mnemonic NEM1 and, for each "." character encountered (except for
C> the initial one), overwrites it with the next corresponding
C> character from NEM2.
C>
C> For example:
C> <pre>
C>     if, on input:    NEM1 = ".DTH...."
C>                      NEM2 = "MXTM    "
C>     then, on output: NEM1 = ".DTHMXTM"
C> </pre>
C>
C> @param[inout] NEM1 - character*8:
C> - on input, a "following value" mnemonic.
C> - on output, a copy of input NEM1 with all "." characters (except the
C> initial one) overwritten with corresponding characters from NEM2.
C> @param[in] NEM2 - character*8: mnemonic immediately following NEM1
C> within DX BUFR table.
C>
C> @author Woollen @date 1994-01-06
      SUBROUTINE RSVFVM(NEM1,NEM2)



      CHARACTER*8 NEM1,NEM2

      DO I=1,LEN(NEM1)
      IF(I.EQ.1) THEN

C        Skip initial "." and initialize J.

         J = 1
      ELSE
         IF(NEM1(I:I).EQ.'.') THEN
            NEM1(I:I) = NEM2(J:J)
            J = J+1
         ENDIF
      ENDIF
      ENDDO

      RETURN
      END
