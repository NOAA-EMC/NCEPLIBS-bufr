C> @file
C> @brief Check whether a data subset will fit within a BUFR message
C>
C> @author J. Ator @date 2009-03-23

C> Check whether the current data subset in the
C> internal arrays will fit within the current BUFR message in the
C> internal arrays, based on the prescribed maximum size of a BUFR
C> message and the allowance of some extra "wiggle room" that may
C> be needed later when writing out the message.
C>
C> @param[in] MSIZ -- integer: Size (in bytes) of current BUFR message
C> @param[in] ITOADD -- integer: Size (in bytes) of current data subset
C> @param[in] MXSIZ -- integer: Maximum size of a BUFR message
C> @returns MSGFULL -- logical: Flag indicating whether the current
C>                     data subset will fit within the current BUFR
C>                     message
C>
C> @author J. Ator @date 2009-03-23
      LOGICAL FUNCTION MSGFULL(MSIZ,ITOADD,MXSIZ)

      use modv_vars, only: maxnc

      COMMON /MSGSTD/ CSMF
      COMMON /TNKRCP/ ITRYR,ITRMO,ITRDY,ITRHR,ITRMI,CTRT

      CHARACTER*1 CSMF
      CHARACTER*1 CTRT

C----------------------------------------------------------------------
C----------------------------------------------------------------------

C     Allow for at least 11 additional bytes of "wiggle room" in the
C     message, because subroutine MSGWRT may do any or all of the
C     following:
C        3 bytes may be added by a call to subroutine CNVED4
C      + 1 byte (at most) of padding may be added to Section 4
C      + 7 bytes (at most) of padding may be added up to the next
C          word boundary after Section 5
C     ----
C       11

      IWGBYT = 11

C     But subroutine MSGWRT may also do any of all of the following:

C        6 bytes may be added by a call to subroutine ATRCPT

         IF(CTRT.EQ.'Y') IWGBYT = IWGBYT + 6

C        (MAXNC*2) bytes (at most) may be added by a call to
C        subroutine STNDRD

         IF(CSMF.EQ.'Y') IWGBYT = IWGBYT + (MAXNC*2)

C     Determine whether the subset will fit.

      IF ( ( MSIZ + ITOADD + IWGBYT ) .GT. MXSIZ ) THEN
        MSGFULL = .TRUE.
      ELSE
        MSGFULL = .FALSE.
      ENDIF

      RETURN
      END
