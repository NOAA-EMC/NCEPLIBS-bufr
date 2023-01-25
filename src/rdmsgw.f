C> @file
C> @brief Read a BUFR message.
C>
C> ### Program History Log
C> Date | Programmer | Comments 
C> -----|------------|----------
C> 2005-11-29 | J. Ator    | Original author.
C> 2009-03-23 | D. Keyser  | Call bort in case of mesg overflow.
C> 2012-09-15 | J. Woollen | C i/o interface; use C routine crdbufr(); remove code which checks sec0 and message length.
C>
C> @author J. Ator @date 2005-11-29
      
C> This subroutine reads the next BUFR message from logical
C> unit LUNIT as an array of integer words.
C>
C> @param[in] LUNIT - integer: fortran logical unit number for BUFR file.
C>
C> @param[out] MESG - integer(*): BUFR message
C> @param[out] IRET - integer: return code:
C> - 0 normal return
C> - -1 end-of-file encountered while reading from LUNIT
C>
C> @author J. Ator @date 2005-11-29
      SUBROUTINE RDMSGW(LUNIT,MESG,IRET)

      USE MODV_MXMSGL

      COMMON /HRDWRD/ NBYTW,NBITW,IORD(8)

      DIMENSION   MESG(*)

      CHARACTER*128 BORT_STR
      INTEGER CRDBUFR

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      CALL STATUS(LUNIT,LUN,IL,IM)
1     IRET=CRDBUFR(LUN,MESG,MXMSGL)
      IF(IRET.eq.-3)
     +   CALL ERRWRT('BUFRLIB: RDMSGW - SKIPPING OVERLARGE MESSAGE')
      IF(IRET.eq.-2)
     +   CALL ERRWRT('BUFRLIB: RDMSGW - SKIPPING CORRUPTED MESSAGE')
      IF(IRET.LT.-1) GOTO 1
      RETURN
      END

