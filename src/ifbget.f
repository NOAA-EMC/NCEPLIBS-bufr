C> @file
C> @brief Check whether there are more data subsets available to be
C> read from a BUFR message

C> This function checks whether there are more data subsets available
C> to be read from within the BUFR message most recently opened for
C> reading via a call to subroutine readmg(), readerme() or equivalent.
C>
C> @author J. Woollen
C> @date 1994-01-06
C>
C> @param[in] LUNIT  - integer: Fortran logical unit number for
C>                         BUFR file
C> @returns ifbget - integer:
C>                    -  0 = there is at least one more data subset
C>                           to be read from the BUFR message
C>                    - -1 = there are no more data subsets
C>                           to be read from the BUFR message
C>
C> <b>Program history log:</b>
C> - 1994-01-06  J. Woollen -- Original author
C> - 1998-07-08  J. Woollen -- Replaced call to Cray library routine ABORT
C>                             with call to new internal routine bort()
C> - 1999-11-18  J. Woollen -- The number of BUFR files which can be
C>                             opened at one time increased from 10 to 32
C>                             (necessary in order to process multiple
C>                             BUFR files under the MPI)
C> - 2014-12-10  J. Ator  -- Use modules instead of COMMON blocks
C>
      FUNCTION IFBGET(LUNIT)

      USE MODA_MSGCWD

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  MAKE SURE A FILE/MESSAGE IS OPEN FOR INPUT
C  ------------------------------------------

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IL.GT.0) GOTO 901
      IF(IM.EQ.0) GOTO 902

C  SEE IF THERE IS ANOTHER SUBSET IN THE MESSAGE
C  ---------------------------------------------

      IF(NSUB(LUN).LT.MSUB(LUN)) THEN
         IFBGET = 0
      ELSE
         IFBGET = -1
      ENDIF

C  EXITS
C  -----

      RETURN
900   CALL BORT('BUFRLIB: IFBGET - INPUT BUFR FILE IS CLOSED, IT '//
     . 'MUST BE OPEN FOR INPUT')
901   CALL BORT('BUFRLIB: IFBGET - INPUT BUFR FILE IS OPEN FOR '//
     . 'OUTPUT, IT MUST BE OPEN FOR INPUT')
902   CALL BORT('BUFRLIB: IFBGET - A MESSAGE MUST BE OPEN IN INPUT '//
     . 'BUFR FILE, NONE ARE')
      END
