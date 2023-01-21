C> @file
C> @brief Pad a bufr message with zeroed-out bytes
C> from the end of the message up to the next 8-byte boundary.	
C> @author Ator @date 2005-11-29
	
C> This subroutine pads a bufr message with zeroed-out bytes
C> from the end of the message up to the next 8-byte boundary.
C>
C> @param[inout] MESG - integer: *-word packed binary array containing
C> bufr message out: *-word packed binary array containing bufr message
C> with npbyt zeroed-out bytes appendebbbd to the end.
C> @param[in] LMESG - integer: dimensioned size (in integer words) of mesg;
C> used by the subroutine to ensure that it does not overflow the mesg array.
C> @param[inout] NPBYT - integer: number of zeroed-out bytes appended to mesg.
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
