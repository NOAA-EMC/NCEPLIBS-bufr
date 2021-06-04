C> @file
C> @brief Get the number of data subsets from a BUFR message.

C> This function returns the total number of data subsets available
C> within the BUFR message that was most recently opened for reading
C> via a call to one of the other
C> [message-reading subroutines](@ref hierarchy)
C> for a specified Fortran logical unit.
C>
C> <p>The data subsets themselves do not need to have already been
C> read via previous calls to any of the
C> [subset-reading subroutines](@ref hierarchy).
C>
C> @author J. Woollen
C> @date 1994-01-06
C>
C> @param[in] LUNIT  - integer: Fortran logical unit number for
C>                     BUFR file
C> @returns nmsub    - integer: Number of data subsets
C>
C> <b>Program history log:</b>
C> - 1994-01-06  J. Woollen -- Original author
C> - 1998-07-08  J. Woollen -- Replaced call to Cray library routine ABORT
C>                             with call to new internal routine bort()
C> - 1999-11-18  J. Woollen -- The number of BUFR files which can be
C>                             opened at one time increased from 10 to 32
C>                             (necessary in order to process multiple
C>                             BUFR files under the MPI)
C> - 2014-12-10  J. Ator    -- Use modules instead of COMMON blocks
C>
      FUNCTION NMSUB(LUNIT)

      USE MODA_MSGCWD

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      NMSUB = 0

C  CHECK THE FILE STATUS
C  ---------------------

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IL.GT.0) GOTO 901
      IF(IM.EQ.0) GOTO 902

      NMSUB = MSUB(LUN)

C  EXITS
C  -----

      RETURN
900   CALL BORT('BUFRLIB: NMSUB - INPUT BUFR FILE IS CLOSED, IT MUST '//
     . 'BE OPEN FOR INPUT')
901   CALL BORT('BUFRLIB: NMSUB - INPUT BUFR FILE IS OPEN FOR OUTPUT,'//
     . ' IT MUST BE OPEN FOR INPUT')
902   CALL BORT('BUFRLIB: NMSUB - A MESSAGE MUST BE OPEN IN INPUT '//
     . 'BUFR FILE, NONE ARE')
      END
