C> @file
C> @brief Get the current message number and data subset number within
C> a BUFR file.
C>
C> @author J. Woollen @date 1994-01-06

C> Get the current message number and data subset number within
C> a BUFR file.
C>
C> This subroutine returns the current location of the file pointer
C> within a BUFR file, in terms of a message number counting from the
C> beginning of the file, and a data subset number counting from the
C> beginning of that message.
C>
C> @remarks
C> - Logical unit LUNIT should have already been opened via a previous
C> call to subroutine openbf(). If LUNIT was opened for input
C> operations, then KMSG is incremented with each call to any of the
C> [message-reading subroutines](@ref hierarchy), and KSUB is
C> incremented with each call to any of the
C> [subset-reading subroutines](@ref hierarchy) for that message.
C> Otherwise, if LUNIT was opened for output operations, then KMSG is
C> incremented with each call to any of the
C> [message-writing subroutines](@ref hierarchy), and KSUB is
C> incremented with each call to any of the
C> [subset-writing subroutines](@ref hierarchy) for that message.
C> - The value returned for KMSG does <b>not</b> include any messages
C> which contain DX BUFR tables information.
C>
C> @param[in] LUNIT - integer: Fortran logical unit number for BUFR file.
C> @param[out] KMSG - integer: Ordinal number of current message,
C> counting from the beginning of the BUFR file, but
C> not counting any messages which contain DX BUFR
C> tables information.
C> @param[out] KSUB - integer: Ordinal number of current data subset
C> within (KMSG)th message, counting from the
C> beginning of the message.
C>
C> @author J. Woollen @date 1994-01-06
      RECURSIVE SUBROUTINE UFBCNT(LUNIT,KMSG,KSUB)

      USE MODA_MSGCWD
      USE MODV_IM8B

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK FOR I8 INTEGERS
C  ---------------------

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84(LUNIT,MY_LUNIT,1)
         CALL UFBCNT(MY_LUNIT,KMSG,KSUB)
         CALL X48(KMSG,KMSG,1)
         CALL X48(KSUB,KSUB,1)

         IM8B=.TRUE.
         RETURN
      ENDIF

C  CHECK THE FILE STATUS - RETURN THE MESSAGE AND SUBSET COUNTERS
C  --------------------------------------------------------------

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900
      KMSG = NMSG(LUN)
      KSUB = NSUB(LUN)

C  EXITS
C  -----

      RETURN
900   CALL BORT('BUFRLIB: UFBCNT - BUFR FILE IS CLOSED, IT MUST BE '//
     . 'OPEN FOR EITHER INPUT OR OUTPUT')
      END
