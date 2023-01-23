C> @file
C> @brief Pad a BUFR message with zeroed-out bytes up to the
C> next 8-byte boundary.
C> @author Ator @date 2005-11-29
	
C> This subroutine pads a BUFR message with zeroed-out bytes
C> from the end of the message up to the next 8-byte boundary.
C>
C> @param[inout] MESG - integer(*):
C>  - on input, contains BUFR message to be padded
C>  - on output, contains BUFR message with NPBYT zeroed-out bytes appended
C>    to the end
C> @param[in] LMESG - integer: dimensioned size (in integer words) of MESG;
C> used by the subroutine to ensure that it does not overflow the MESG array.
C> @param[out] NPBYT - integer: number of zeroed-out bytes appended to MESG.
C>
C> @author Ator @date 2005-11-29
	SUBROUTINE PADMSG(MESG,LMESG,NPBYT)

	COMMON /HRDWRD/ NBYTW,NBITW,IORD(8)

	DIMENSION MESG(*)

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C	Make sure that the array is big enough to hold the additional
C	byte padding that will be appended to the end of the message.

	NMW = NMWRD(MESG)
	IF(NMW.GT.LMESG) GOTO 900

C	Pad from the end of the message up to the next 8-byte boundary.

	NMB = IUPBS01(MESG,'LENM')
	IBIT = NMB*8
	NPBYT = ( NMW * NBYTW ) - NMB
	DO I = 1, NPBYT
	    CALL PKB(0,8,MESG,IBIT)
	ENDDO

	RETURN
900     CALL BORT('BUFRLIB: PADMSG - CANNOT ADD PADDING TO MESSAGE '//
     .    'ARRAY; TRY A LARGER DIMENSION FOR THIS ARRAY')
	END
