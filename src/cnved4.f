C> @file
C> @brief Convert a BUFR edition 3 message to BUFR edition 4.

C> This subroutine reads an input BUFR message encoded using BUFR
C> edition 3 and outputs an equivalent BUFR message encoded using
C> BUFR edition 4.
C>
C> <p>This subroutine performs the same function as subroutine pkvs01()
C> when the latter is called with S01MNEM = 'BEN' and IVAL = 4, except
C> that the latter subroutine operates on BUFR messages internally
C> within the software, whereas this subroutine operates on a single
C> BUFR message passed in via a memory array.
C>
C> @author J. Ator
C> @date 2005-11-29
C>
C> @param[in] MSGIN   -- integer(*): BUFR message
C> @param[in] LMSGOT  -- integer: Dimensioned size (in integers) of
C>                       MSGOT; used by the subroutine to ensure that
C>                       it doesn't overflow the MSGOT array
C> @param[out] MSGOT  -- integer(*): Copy of MSGIN encoded using
C>                       BUFR edition 4
C>
C> @remarks
C> - MSGIN and MSGOT must be separate arrays.
C> - BUFR edition 4 messages are usually longer in length than their
C> BUFR edition 3 counterparts, so it's usually a good idea to allow
C> for extra space when allocating MSGOT within the application program.
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 2005-11-29 | J. Ator | Original author |
C> | 2009-08-12 | J. Ator | Allow silent return (instead of bort() return) if MSGIN is already encoded using edition 4 |
C>
	SUBROUTINE CNVED4(MSGIN,LMSGOT,MSGOT)

	DIMENSION MSGIN(*), MSGOT(*)

	COMMON /HRDWRD/ NBYTW,NBITW,IORD(8)

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

	IF(IUPBS01(MSGIN,'BEN').EQ.4) THEN

C	  The input message is already encoded using edition 4, so just
C	  copy it from MSGIN to MSGOT and then return.

	  NMW = NMWRD(MSGIN)
	  IF(NMW.GT.LMSGOT) GOTO 900 
	  DO I = 1, NMW
	    MSGOT(I) = MSGIN(I)
	  ENDDO
	  RETURN
	ENDIF

C	Get some section lengths and addresses from the input message.

	CALL GETLENS(MSGIN,3,LEN0,LEN1,LEN2,LEN3,L4,L5)

	IAD2 = LEN0 + LEN1
	IAD4 = IAD2 + LEN2 + LEN3 

	LENM = IUPBS01(MSGIN,'LENM')

C	Check for overflow of the output array.  Note that the new
C	edition 4 message will be a total of 3 bytes longer than the
C	input message (i.e. 4 more bytes in Section 1, but 1 fewer
C	byte in Section 3).

	LENMOT = LENM + 3
	IF(LENMOT.GT.(LMSGOT*NBYTW)) GOTO 900 

	LEN1OT = LEN1 + 4
	LEN3OT = LEN3 - 1

C	Write Section 0 of the new message into the output array.

	CALL MVB ( MSGIN, 1, MSGOT, 1, 4 )
	IBIT = 32
	CALL PKB ( LENMOT, 24, MSGOT, IBIT )
	CALL PKB ( 4, 8, MSGOT, IBIT )

C	Write Section 1 of the new message into the output array.

	CALL PKB ( LEN1OT, 24, MSGOT, IBIT )
	CALL PKB ( IUPBS01(MSGIN,'BMT'), 8, MSGOT, IBIT )
	CALL PKB ( IUPBS01(MSGIN,'OGCE'), 16, MSGOT, IBIT )
	CALL PKB ( IUPBS01(MSGIN,'GSES'), 16, MSGOT, IBIT )
	CALL PKB ( IUPBS01(MSGIN,'USN'), 8, MSGOT, IBIT )
	CALL PKB ( IUPBS01(MSGIN,'ISC2')*128, 8, MSGOT, IBIT )
	CALL PKB ( IUPBS01(MSGIN,'MTYP'), 8, MSGOT, IBIT )

C	Set a default of 255 for the international subcategory.

	CALL PKB ( 255, 8, MSGOT, IBIT )
	CALL PKB ( IUPBS01(MSGIN,'MSBT'), 8, MSGOT, IBIT )
	CALL PKB ( IUPBS01(MSGIN,'MTV'), 8, MSGOT, IBIT )
	CALL PKB ( IUPBS01(MSGIN,'MTVL'), 8, MSGOT, IBIT )
	CALL PKB ( IUPBS01(MSGIN,'YEAR'), 16, MSGOT, IBIT )
	CALL PKB ( IUPBS01(MSGIN,'MNTH'), 8, MSGOT, IBIT )
	CALL PKB ( IUPBS01(MSGIN,'DAYS'), 8, MSGOT, IBIT )
	CALL PKB ( IUPBS01(MSGIN,'HOUR'), 8, MSGOT, IBIT )
	CALL PKB ( IUPBS01(MSGIN,'MINU'), 8, MSGOT, IBIT )

C	Set a default of 0 for the second.

	CALL PKB ( 0, 8, MSGOT, IBIT )

C	Copy Section 2 (if it exists) through the next-to-last byte
C	of Section 3 from the input array to the output array.

	CALL MVB ( MSGIN, IAD2+1, MSGOT, (IBIT/8)+1, LEN2+LEN3-1 )

C	Store the length of the new Section 3.

	IBIT = ( LEN0 + LEN1OT + LEN2 ) * 8
	CALL PKB ( LEN3OT, 24, MSGOT, IBIT )
	
C	Copy Section 4 and Section 5 from the input array to the
C	output array.

	IBIT = IBIT + ( LEN3OT * 8 ) - 24
	CALL MVB ( MSGIN, IAD4+1, MSGOT, (IBIT/8)+1, LENM-IAD4 )

	RETURN
900	CALL BORT('BUFRLIB: CNVED4 - OVERFLOW OF OUTPUT (EDITION 4) '//
     .    'MESSAGE ARRAY; TRY A LARGER DIMENSION FOR THIS ARRAY')
	END
