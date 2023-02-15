C> @file
C> @brief Get information about a BUFR message
C>
C> @author J. Woollen @date 1994-01-06

C> This subroutine reads through a BUFR file (starting from the beginning
C> of the file) and returns the message type (from Section 1) of the
C> first message it encounters which does not contain DX BUFR table
C> information.
C>
C> The BUFR file should not have already been opened via a call
C> to subroutine openbf(); however, it should already be associated
C> with Fortran logical unit number LUNIT.
C>
C> This subroutine is similar to subroutine mesgbc(), except that
C> this subroutine will only skip past DX BUFR table messages at the
C> beginning of a file, whereas mesgbc() will also skip past any "dummy"
C> messages containing the dump center time or dump initiation time
C> within NCEP dump files.  Furthermore, mesgbc() also returns a
C> message compression indicator, and it also has an option to operate
C> on a BUFR message that has already been read into the internal arrays.
C>
C> @param[in] LUNIT - integer: Fortran logical unit number for BUFR file
C> @param[out] MESGTYP - integer: Message type
C>                        - -1 = error reading the BUFR file, or no
C>                               messages were read from the file
C>                        - 11 = BUFR file only contained DX BUFR table
C>                               messages
C>
C> @author J. Woollen @date 1994-01-06

      RECURSIVE SUBROUTINE MESGBF(LUNIT,MESGTYP)

      USE MODA_MGWA
      USE MODV_IM8B

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK FOR I8 INTEGERS
C  ---------------------

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84(LUNIT,MY_LUNIT,1)
         CALL MESGBF(MY_LUNIT,MESGTYP)
         CALL X48(MESGTYP,MESGTYP,1)

         IM8B=.TRUE.
         RETURN
      ENDIF

      MESGTYP = -1

C  SINCE OPENBF HAS NOT YET BEEN CALLED, CALL IT
C  ---------------------------------------------

      CALL OPENBF(LUNIT,'INX',LUNIT)

C  READ PAST ANY BUFR TABLES AND RETURN THE FIRST MESSAGE TYPE FOUND
C  -----------------------------------------------------------------

1     CALL RDMSGW(LUNIT,MGWA,IER)
      IF(IER.EQ.0) THEN
         MESGTYP = IUPBS01(MGWA,'MTYP')
         IF(IDXMSG(MGWA).EQ.1) GOTO 1
      ENDIF

C  CLOSE THE FILE
C  --------------

      CALL CLOSBF(LUNIT)

C  EXIT
C  ----

100   RETURN
      END
