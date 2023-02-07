C> @file
C> @brief Search for a specified occurrence of a specified mnemonic within
C> a data subset definition, starting from a specified location.
C>
C> ### Program History Log
C> Date | Programmer | Comments
C> -----|------------|----------
C> 2014-10-02 | J. Ator | Original author.
C> 2014-12-10 | J. Ator | Use modules instead of common blocks.
C>
C> @author J Ator @date 2014-10-02

C> This subroutine finds the (NUTAG)th occurrence of mnemonic
C> UTAG within the current overall subset definition, starting from
C> parameter #(NIN) within the subset. The subroutine searches forward
C> from NIN if NUTAG is positive or else backward if NUTAG is negative.
C>
C> @param[in] LUN - integer: I/O stream index into internal memory arrays.
C> @param[in] UTAG - character*(*): mnemonic.
C> @param[in] NUTAG - integer: ordinal occurrence of UTAG to search for within
C>     the overall subset definition, counting from parameter #(NIN) within the subset.
C>     The subroutine will search in a forward direction from parameter #(NIN) if
C>     NUTAG is positive or else in a backward direction if NUTAG is negative.
C> @param[in] NIN - integer: location within the overall subset definition from which to begin searching for UTAG.
C> @param[out] NOUT - integer: location of (NUTAG)th occurrence of UTAG.
C> @param[out] IRET - integer: return code.
C> - 0 Normal return.
C> - -1 Requested mnemonic could not be found, or some other error occurred.
C>
C> @author J Ator @date 2014-10-02
        SUBROUTINE FSTAG ( LUN, UTAG, NUTAG, NIN, NOUT, IRET )

        USE MODA_USRINT
        USE MODA_TABLES

        CHARACTER*10  TGS(15)

        CHARACTER*(*) UTAG

        DATA MAXTG  /15/

C----------------------------------------------------------------------
C----------------------------------------------------------------------

        IRET = -1

C       Confirm that there is only one mnemonic in the input string.

        CALL PARSTR( UTAG, TGS, MAXTG, NTG, ' ', .TRUE. )
        IF ( NTG .NE .1 ) RETURN

C       Starting from NIN, search either forward or backward for the
C       (NUTAG)th occurrence of UTAG.

        IF ( NUTAG .EQ. 0 ) RETURN
        ISTEP = ISIGN( 1, NUTAG )
        ITAGCT = 0
        NOUT = NIN + ISTEP
        DO WHILE ( ( NOUT .GE. 1 ) .AND. ( NOUT .LE. NVAL(LUN) ) )
            IF ( TGS(1) .EQ. TAG(INV(NOUT,LUN)) ) THEN
                ITAGCT = ITAGCT + 1
                IF ( ITAGCT .EQ. IABS(NUTAG) ) THEN
                    IRET = 0
                    RETURN
                ENDIF
            ENDIF
            NOUT = NOUT + ISTEP
        ENDDO

        RETURN
        END
