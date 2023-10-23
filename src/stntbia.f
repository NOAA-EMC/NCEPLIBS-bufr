C> @file
C> @brief Store a new entry within the internal BUFR Table A.
C> @author Ator @date 2009-03-23

C> Store a new entry within internal BUFR Table A.
C>
C> @param[in] N - integer: storage index into internal Table A.
C> @param[in] LUN - integer: I/O stream index into internal Table A.
C> @param[in] NUMB - character*6: FXY number for new Table A entry (in format FXXYYY).
C> @param[in] NEMO - character*8: mnemonic corresponding to NUMB.
C> @param[in] CELSQ - character*55: sequence description corresponding to NUMB.
C>
C> @author Ator @date 2009-03-23
        SUBROUTINE STNTBIA ( N, LUN, NUMB, NEMO, CELSQ )

        USE MODA_TABABD

        CHARACTER*128 BORT_STR

        CHARACTER*(*) NUMB, NEMO, CELSQ

        LOGICAL DIGIT

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C       Confirm that neither NEMO nor NUMB has already been defined
C       within the internal BUFR Table A (in COMMON /TABABD/) for
C       the given LUN.

        DO N=1,NTBA(LUN)
          IF(NUMB(4:6).EQ.TABA(N,LUN)(1: 3)) GOTO 900
          IF(NEMO(1:8).EQ.TABA(N,LUN)(4:11)) GOTO 901
        ENDDO

C       Store the values within the internal BUFR Table A.

        TABA(N,LUN)( 1: 3) = NUMB(4:6)
        TABA(N,LUN)( 4:11) = NEMO(1:8)
        TABA(N,LUN)(13:67) = CELSQ(1:55)

C       Decode and store the message type and subtype.

        IF ( DIGIT ( NEMO(3:8) ) ) THEN
c  ....   Message type & subtype obtained directly from Table A mnemonic
            READ ( NEMO,'(2X,2I3)') MTYP, MSBT
            IDNA(N,LUN,1) = MTYP
            IDNA(N,LUN,2) = MSBT
        ELSE
c  ....   Message type obtained from Y value of Table A seq. descriptor
            READ ( NUMB(4:6),'(I3)') IDNA(N,LUN,1)
c  ....   Message subtype hardwired to ZERO
            IDNA(N,LUN,2) = 0
        ENDIF

C       Update the count of internal Table A entries.

        NTBA(LUN) = N

        RETURN
900     WRITE(BORT_STR,'("BUFRLIB: STNTBIA - TABLE A FXY VALUE (",A,") '
     .     //'HAS ALREADY BEEN DEFINED (DUPLICATE)")') NUMB
        CALL BORT(BORT_STR)
901     WRITE(BORT_STR,'("BUFRLIB: STNTBIA - TABLE A MNEMONIC (",A,") '
     .     //'HAS ALREADY BEEN DEFINED (DUPLICATE)")') NEMO
        CALL BORT(BORT_STR)
        END
