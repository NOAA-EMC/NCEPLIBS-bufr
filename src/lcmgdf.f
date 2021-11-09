C> @file
C> @brief Check for the existence of any long character strings
C> (greater than 8 bytes) within a data subset definition.

C> This function checks whether the subset definition for a given
C> message type contains any long character strings (greater
C> than 8 bytes).
C>
C> @author J. Ator
C> @date 2009-07-09
C>
C> @param[in] LUNIT -- integer: Fortran logical unit number for
C>                     BUFR file
C> @param[in] SUBSET -- character*8: Table A mnemonic of message
C>                      type to be checked
C> @returns lcmgdf -- integer: 
C>                     -  0 = SUBSET does not contain any long
C>                            character strings
C>                     -  1 = SUBSET contains at least one long
C>                            character string
C>
C> @remarks
C> - LUNIT may be open for either input or output operations via a
C> previous call to subroutine openbf().  However, in either case,
C> SUBSET must already be defined within the BUFR tables that are
C> associated with LUNIT, typically as [DX BUFR tables](@ref dfbftab)
C> information supplied via argument LUNDX when openbf() was called,
C> or, if openbf() was called with IO = 'SEC3', then as
C> [master BUFR table](@ref dfbfmstab) information during a previous
C> call to one of the [message-reading subroutines](@ref hierarchy).
C> - Note that this function does not return mnemonic(s) associated
C> with any long character string(s) found within SUBSET; rather,
C> it only checks whether at least one such mnemonic exists.  If any
C> are found, the application program can process them via a
C> separate call to subroutine readlc() (when reading BUFR data
C> subsets) or subroutine writlc() (when writing BUFR data subsets).
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 2009-07-09 | J. Ator | Original author |
C> | 2014-12-10 | J. Ator | Use modules instead of COMMON blocks |
C>
      INTEGER FUNCTION LCMGDF(LUNIT,SUBSET)

      USE MODA_TABLES

      CHARACTER*8  SUBSET

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C     Get LUN from LUNIT.

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF (IL.EQ.0) GOTO 900
 
C     Confirm that SUBSET is defined for this logical unit.

      CALL NEMTBA(LUN,SUBSET,MTYP,MSBT,INOD)

C     Check if there's a long character string in the definition.

      NTE = ISC(INOD)-INOD

      DO I = 1, NTE
        IF ( (TYP(INOD+I).EQ.'CHR') .AND. (IBT(INOD+I).GT.64) ) THEN
          LCMGDF = 1
          RETURN
        ENDIF
      ENDDO

      LCMGDF = 0

      RETURN
900   CALL BORT('BUFRLIB: LCMGDF - INPUT BUFR FILE IS CLOSED, IT MUST'//
     . ' BE OPEN')
      END
