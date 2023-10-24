C> @file
C> @brief Store a new entry within the internal BUFR Table B or D.
C> @author Ator @date 2009-03-23

C> Store a new entry within internal BUFR
C> Table B or D, depending on the value of NUMB.
C>
C> @param[in] N - integer: storage index into internal Table B or D.
C> @param[in] LUN - integer: File ID.
C> @param[in] NUMB - character*6: FXY number for new Table B or D entry
C> (in format FXXYYY).
C> @param[in] NEMO - character*8: mnemonic corresponding to NUMB.
C> @param[in] CELSQ - character*55: element or sequence description
C> corresponding to NUMB.
C>
C> @author Ator @date 2009-03-23
        SUBROUTINE STNTBI ( N, LUN, NUMB, NEMO, CELSQ )

        USE MODA_TABABD

        CHARACTER*(*) NUMB, NEMO, CELSQ

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

        CALL NENUBD ( NEMO, NUMB, LUN )

        IF ( NUMB(1:1) .EQ. '0') THEN
            IDNB(N,LUN) = IFXY(NUMB)
            TABB(N,LUN)( 1: 6) = NUMB(1:6)
            TABB(N,LUN)( 7:14) = NEMO(1:8)
            TABB(N,LUN)(16:70) = CELSQ(1:55)
            NTBB(LUN) = N
        ELSE IF ( NUMB(1:1) .EQ. '3') THEN
            IDND(N,LUN) = IFXY(NUMB)
            TABD(N,LUN)( 1: 6) = NUMB(1:6)
            TABD(N,LUN)( 7:14) = NEMO(1:8)
            TABD(N,LUN)(16:70) = CELSQ(1:55)
            NTBD(LUN) = N
        ENDIF

        RETURN
        END
