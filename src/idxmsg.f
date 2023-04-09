C> @file
C> @brief Check whether a BUFR message contains DX BUFR tables
C> information.
C>
C> @author J. Ator @date 2009-03-23

C> Check whether a BUFR message contains DX BUFR tables
C> information.
C>
C> This function determines whether a given BUFR message contains
C> DX BUFR tables information that was generated by the BUFRLIB
C> software.
C>
C> @param[in] MESG - integer(*): BUFR message.
C>
C> @returns Flag indicating whether MESG
C> contains DX BUFR tables information:
C> - 0 = No
C> - 1 = Yes
C>
C> @author J. Ator @date 2009-03-23
        RECURSIVE FUNCTION IDXMSG( MESG ) RESULT( IRET )

        USE MODV_IM8B

        DIMENSION MESG(*)

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C       Check for I8 integers.

        IF(IM8B) THEN
            IM8B=.FALSE.

            IRET = IDXMSG( MESG )

            IM8B=.TRUE.
            RETURN
        END IF

C       Note that the following test relies upon logic within subroutine
C       DXMINI which zeroes out the Section 1 date of all DX dictionary
C       messages.

        IF ( (IUPBS01(MESG,'MTYP').EQ.11) .AND.
     .       (IUPBS01(MESG,'MNTH').EQ.0) .AND.
     .       (IUPBS01(MESG,'DAYS').EQ.0) .AND.
     .       (IUPBS01(MESG,'HOUR').EQ.0) ) THEN
           IRET = 1
        ELSE
           IRET = 0
        END IF

        RETURN
        END
