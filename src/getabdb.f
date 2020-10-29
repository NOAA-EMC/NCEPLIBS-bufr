C> @file
C> @author ATOR @date 2005-11-29
      
C> THIS SUBROUTINE RETURNS INTERNAL TABLE B AND TABLE D
C>   INFORMATION FOR LOGICAL UNIT LUNIT IN A PRE-DEFINED ASCII FORMAT.
C>
C> PROGRAM HISTORY LOG:
C> 2005-11-29  J. ATOR    -- ADDED TO BUFR ARCHIVE LIBRARY (WAS IN-LINED
C>                           IN PROGRAM NAMSND)
C> 2014-12-10  J. ATOR    -- USE MODULES INSTEAD OF COMMON BLOCKS
C>
C> USAGE:    CALL GETABDB( LUNIT, TABDB, ITAB, JTAB )
C>   INPUT ARGUMENT LIST:
C>     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE
C>     ITAB     - INTEGER: DIMENSIONED SIZE OF TABDB ARRAY
C>
C>   OUTPUT ARGUMENT LIST:
C>     TABDB    - CHARACTER*128: (JTAB)-WORD ARRAY OF INTERNAL TABLE B
C>                AND TABLE D INFORMATION
C>     JTAB     - INTEGER: NUMBER OF ENTRIES STORED WITHIN TABDB
C>
C> REMARKS:
C>    THIS ROUTINE CALLS:        NEMTBD   STATUS
C>    THIS ROUTINE IS CALLED BY: None
C>                               Normally called only by application
C>                               programs.
C>
      SUBROUTINE GETABDB(LUNIT,TABDB,ITAB,JTAB)



      USE MODA_TABABD
      USE MODA_NMIKRP

      INCLUDE 'bufrlib.inc'

      CHARACTER*128 TABDB(*)
      CHARACTER*8   NEMO

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      JTAB = 0

C  MAKE SURE THE FILE IS OPEN
C  --------------------------

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) RETURN

C  WRITE OUT THE TABLE D ENTRIES FOR THIS FILE
C  -------------------------------------------

      DO I=1,NTBD(LUN)
      NEMO = TABD(I,LUN)(7:14)
      CALL NEMTBD(LUN,I,NSEQ,NEM(1,1),IRP(1,1),KRP(1,1))
      DO J=1,NSEQ,10
      JTAB = JTAB+1
      IF(JTAB.LE.ITAB) THEN
         WRITE(TABDB(JTAB),1) NEMO,(NEM(K,1),K=J,MIN(J+9,NSEQ))
1        FORMAT('D ',A8,10(1X,A10))
      ENDIF
      ENDDO
      ENDDO

C  ADD THE TABLE B ENTRIES
C  -----------------------

      DO I=1,NTBB(LUN)
      JTAB = JTAB+1
      IF(JTAB.LE.ITAB) THEN
         WRITE(TABDB(JTAB),2) TABB(I,LUN)(7:14),TABB(I,LUN)(71:112)
2        FORMAT('B ',A8,1X,A42)
      ENDIF
      ENDDO

      RETURN
      END
