C> @file
C> @author WOOLLEN @date 2002-05-14
      
C> THIS SUBROUTINE SEARCHES FOR AN INTEGER IDN, CONTAINING THE
C>   BIT-WISE REPRESENTATION OF A DESCRIPTOR (FXY) VALUE, WITHIN THE
C>   INTERNAL BUFR TABLE B AND D ARRAYS IN MODULE TABABD.  IF FOUND,
C>   IT RETURNS THE CORRESPONDING MNEMONIC AND OTHER INFORMATION FROM
C>   WITHIN THESE ARRAYS.  IF IDN IS NOT FOUND, IT RETURNS WITH IRET=0.
C>
C> PROGRAM HISTORY LOG:
C> 2002-05-14  J. WOOLLEN -- ORIGINAL AUTHOR
C> 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C>                           INTERDEPENDENCIES
C> 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED
C>                           DOCUMENTATION (INCLUDING HISTORY)
C> 2009-04-21  J. ATOR    -- USE IFXY FOR MORE EFFICIENT SEARCHING
C> 2014-12-10  J. ATOR    -- USE MODULES INSTEAD OF COMMON BLOCKS
C>
C> USAGE:    CALL NUMTBD (LUN, IDN, NEMO, TAB, IRET)
C>   INPUT ARGUMENT LIST:
C>     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
C>     IDN      - INTEGER: BIT-WISE REPRESENTATION OF DESCRIPTOR (FXY)
C>                VALUE
C>
C>   OUTPUT ARGUMENT LIST:
C>     NEMO     - CHARACTER*(*): MNEMONIC CORRESPONDING TO IDN
C>     TAB      - CHARACTER*1: TYPE OF FXY VALUE THAT IS BIT-WISE
C>                REPRESENTED BY IDN:
C>                     'B' = BUFR Table B descriptor
C>                     'D' = BUFR Table D descriptor
C>     IRET     - INTEGER: RETURN VALUE (SEE REMARKS)
C>
C> REMARKS:
C>    THE INTERPRETATION OF THE RETURN VALUE IRET DEPENDS UPON THE
C>    RETURN VALUE OF TAB, AS FOLLOWS:
C>
C>    IF ( TAB = 'B' ) THEN
C>       IRET = positional index of IDN within internal BUFR Table B
C>              array
C>    ELSE IF ( TAB = 'D') THEN
C>       IRET = positional index of IDN within internal BUFR Table D
C>              array
C>    ELSE IF ( IRET = 0 ) THEN
C>       IDN was not found in internal BUFR Table B or D
C>    END IF
C>
C>
C>    THIS ROUTINE CALLS:        IFXY
C>    THIS ROUTINE IS CALLED BY: GETCFMNG NUMTAB   RESTD    STSEQ
C>                               UFDUMP
C>                               Normally not called by any application
C>                               programs.
C>
      SUBROUTINE NUMTBD(LUN,IDN,NEMO,TAB,IRET)



      USE MODA_TABABD

      INCLUDE 'bufrlib.inc'

      CHARACTER*(*) NEMO
      CHARACTER*1   TAB

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      NEMO = ' '
      IRET = 0
      TAB = ' '

      IF(IDN.GE.IFXY('300000')) THEN

C        LOOK FOR IDN IN TABLE D
C        -----------------------

         DO I=1,NTBD(LUN)
            IF(IDN.EQ.IDND(I,LUN)) THEN
               NEMO = TABD(I,LUN)(7:14)
               TAB  = 'D'
               IRET = I
               GOTO 100
            ENDIF
         ENDDO

      ELSE

C        LOOK FOR IDN IN TABLE B
C        -----------------------

         DO I=1,NTBB(LUN)
            IF(IDN.EQ.IDNB(I,LUN)) THEN
               NEMO = TABB(I,LUN)(7:14)
               TAB  = 'B'
               IRET = I
               GOTO 100
            ENDIF
         ENDDO

      ENDIF

C  EXIT
C  ----

100   RETURN
      END
