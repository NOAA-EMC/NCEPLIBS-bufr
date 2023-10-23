C> @file
C> @brief Add a tank receipt time to a BUFR message.
C> @author J. Ator @date 2009-03-23

C> Read an input message and output an equivalent
C> BUFR message with a tank receipt time added to Section 1.
C>
C> The tank receipt time to be added must have been specified via
C> a previous call to subroutine strcpt(). This subroutine performs
C> the same function as subroutine strcpt() when the latter is called
C> with CF = 'Y', except that the latter subroutine operates on BUFR
C> messages internally within the software, whereas this subroutine
C> operates on a single BUFR message passed in via a memory array.
C>
C> @remarks
C> - MSGIN and MSGOT must be separate arrays.
C> - MSGOT will be longer in length than MSGIN, so the user must allow
C> for extra space when allocating MSGOT within the application program.
C>
C> @param[in] MSGIN - integer(*): BUFR message
C> @param[in] LMSGOT - integer: Dimensioned size (in integers) of
C> MSGOT; used by the subroutine to ensure that
C> it doesn't overflow the MSGOT array
C> @param[out] MSGOT - integer(*): Copy of MSGIN with a tank
C> receipt time added to Section 1
C>
C> @author J. Ator @date 2009-03-23

        RECURSIVE SUBROUTINE ATRCPT(MSGIN,LMSGOT,MSGOT)

        USE MODV_IM8B

        DIMENSION MSGIN(*), MSGOT(*)

        COMMON /HRDWRD/ NBYTW,NBITW,IORD(8)
        COMMON /TNKRCP/ ITRYR,ITRMO,ITRDY,ITRHR,ITRMI,CTRT

        CHARACTER*1 CTRT

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C       Check for I8 integers.

        IF(IM8B) THEN
           IM8B=.FALSE.

           CALL X84 ( LMSGOT, MY_LMSGOT, 1 )
           CALL ATRCPT ( MSGIN, MY_LMSGOT*2, MSGOT )

           IM8B=.TRUE.
           RETURN
        ENDIF

C       Get some section lengths and addresses from the input message.

        CALL GETLENS(MSGIN,1,LEN0,LEN1,L2,L3,L4,L5)

        IAD1 = LEN0
        IAD2 = IAD1 + LEN1

        LENM = IUPBS01(MSGIN,'LENM')

C       Check for overflow of the output array.  Note that the new
C       message will be 6 bytes longer than the input message.

        LENMOT = LENM + 6
        IF(LENMOT.GT.(LMSGOT*NBYTW)) GOTO 900

        LEN1OT = LEN1 + 6

C       Write Section 0 of the new message into the output array.

        CALL MVB ( MSGIN, 1, MSGOT, 1, 4 )
        IBIT = 32
        CALL PKB ( LENMOT, 24, MSGOT, IBIT )
        CALL MVB ( MSGIN, 8, MSGOT, 8, 1 )

C       Store the length of the new Section 1.

        IBIT = IAD1*8
        CALL PKB ( LEN1OT, 24, MSGOT, IBIT )

C       Copy the remainder of Section 1 from the input array to the
C       output array.

        CALL MVB ( MSGIN, IAD1+4, MSGOT, (IBIT/8)+1, LEN1-3 )

C       Append the tank receipt time data to the new Section 1.

        IBIT = IAD2*8
        CALL PKB ( ITRYR, 16, MSGOT, IBIT )
        CALL PKB ( ITRMO,  8, MSGOT, IBIT )
        CALL PKB ( ITRDY,  8, MSGOT, IBIT )
        CALL PKB ( ITRHR,  8, MSGOT, IBIT )
        CALL PKB ( ITRMI,  8, MSGOT, IBIT )

C       Copy Sections 2, 3, 4 and 5 from the input array to the
C       output array.

        CALL MVB ( MSGIN, IAD2+1, MSGOT, (IBIT/8)+1, LENM-IAD2 )

        RETURN
900     CALL BORT('BUFRLIB: ATRCPT - OVERFLOW OF OUTPUT MESSAGE '//
     .    'ARRAY; TRY A LARGER DIMENSION FOR THIS ARRAY')
        END
