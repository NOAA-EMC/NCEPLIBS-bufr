C> @file
C> @brief Copy an entire BUFR file.

C> This subroutine copies an entire BUFR file from one Fortran
C> logical unit to another.
C>
C> @author J. Woollen
C> @date 1994-01-06
C>
C> @param[in] LUNIN   -- integer: Fortran logical unit number for
C>                       source BUFR file 
C> @param[in] LUNOT   -- integer: Fortran logical unit number for
C>                       target BUFR file 
C>
C> <p>The logical unit numbers LUNIN and LUNOT must already be
C> associated with actual filenames on the local system, typically
C> via a Fortran "OPEN" statement.
C>
C> @remarks
C> - This subroutine uses subroutine msgwrt() to copy each BUFR
C> message from LUNIN to LUNOT; therefore, this subroutine can be
C> used to create a copy of LUNIN where each corresponding message
C> in LUNOT contains any or all of the updates described in the
C> documentation for subroutine msgwrt().
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 1994-01-06 | J. Woollen | Original author |
C> | 1998-07-08 | J. Woollen | Replaced call to Cray library routine ABORT with call to new internal routine bort() |
C> | 2000-09-19 | J. Woollen | Maximum message length increased from 10,000 to 20,000 bytes |
C> | 2004-08-09 | J. Ator    | Maximum message length increased from 20,000 to 50,000 bytes |
C> | 2005-11-29 | J. Ator    | Use rdmsgw() and iupbs01() |
C> | 2012-09-15 | J. Woollen | Modified for C/I/O/BUFR interface; use status() |
C> | 2014-12-10 | J. Ator    | Use modules instead of COMMON blocks |
C>
      SUBROUTINE COPYBF(LUNIN,LUNOT)

      USE MODA_MGWA

      use subroutine_closbf
      use subroutine_openbf

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CALL SUBROUTINE WRDLEN TO INITIALIZE SOME IMPORTANT INFORMATION
C  ABOUT THE LOCAL MACHINE (IN CASE IT HAS NOT YET BEEN CALLED)
C  ---------------------------------------------------------------

      CALL WRDLEN

C  CHECK BUFR FILE STATUSES
C  ------------------------

      CALL STATUS(LUNIN,LUN,IL,IM)
      IF(IL.NE.0) GOTO 900
      CALL STATUS(LUNOT,LUN,IL,IM)
      IF(IL.NE.0) GOTO 901

C  CONNECT THE FILES FOR READING/WRITING TO THE C-I-O INTERFACE 
C  ------------------------------------------------------------

      CALL OPENBF(LUNIN,'INX',LUNIN)
      CALL OPENBF(LUNOT,'OUX',LUNIN)

C  READ AND COPY A BUFR FILE ON UNIT LUNIN TO UNIT LUNOT
C  -----------------------------------------------------

1     CALL RDMSGW(LUNIN,MGWA,IER)
      IF(IER.EQ.0) THEN      
         CALL MSGWRT(LUNOT,MGWA,IUPBS01(MGWA,'LENM'))
         GOTO 1
      ENDIF

C  FREE UP THE FILE CONNECTIONS FOR THE TWO FILES
C  ----------------------------------------------

      CALL CLOSBF(LUNIN)
      CALL CLOSBF(LUNOT) 

C  EXITS
C  -----

      RETURN
900   CALL BORT
     . ('BUFRLIB: COPYBF - INPUT BUFR FILE IS OPEN, IT MUST BE CLOSED')
901   CALL BORT
     . ('BUFRLIB: COPYBF - OUTPUT BUFR FILE IS OPEN, IT MUST BE CLOSED')
      END
