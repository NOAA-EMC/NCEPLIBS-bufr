C> @file
C> @brief Get information about a BUFR message

C> This subroutine examines a BUFR message and returns both the
C> message type (from Section 1) and message compression indicator
C> (from Section 3).
C>
C> @author D. Keyser @date 2003-11-04
C>
C> <p>The message to be examined is obtained in one of two different
C> ways, depending on the sign of LUNIN:
C> - If LUNIN > 0, the subroutine reads and examines Section 1 of each
C> message in a BUFR file starting from the beginning of the file, and
C> continuing until it reaches the first message which actually contains
C> report data.  This means it will skip any messages containing DX BUFR
C> table information, as well as any "dummy" messages containing the
C> dump center time or dump initiation time within NCEP dump files.
C> It then returns the message type and compression indicator from the
C> first message containing report data.  When used this way, the
C> BUFR file in question should not have already been opened via a call
C> to subroutine openbf(), though it should already be associated with
C> Fortran logical unit number LUNIN.  In this situation, the
C> subroutine is similar to subroutine mesgbf(), except that mesgbf()
C> doesn't skip past any "dummy" messages within NCEP dump files, nor
C> does it return a compression indicator.
C> - If LUNIN < 0, the subroutine simply returns the message type and
C> compression indicator for the BUFR message currently stored in the
C> internal arrays via the most recent call to one of the
C> [message-reading subroutines](@ref hierarchy) for Fortran logical
C> unit number ABS(LUNIN).
C>
C> @param[in] LUNIN -- integer: Absolute value is Fortran logical unit
C>                     number for BUFR file
C> @param[out] MESGTYP -- integer: Message type
C>                          - When LUNIN > 0, a MESGTYP value of -256
C>                            means that there was an error reading the
C>                            BUFR file, or that no messages were read from
C>                            the file.  Otherwise, any other MESGTYP
C>                            value < 0 means that none of the messages
C>                            in the BUFR file contained any report data.
C> @param[out] ICOMP -- integer: Message compression indicator
C>                         - -3 = BUFR file does not exist
C>                         - -2 = none of the messages in the BUFR file
C>                                contained any report data
C>                         - -1 = error reading the BUFR file
C>                         -  0 = message is not compressed
C>                         -  1 = message is compressed
C>
C> @author D. Keyser @date 2003-11-04
      RECURSIVE SUBROUTINE MESGBC(LUNIN,MESGTYP,ICOMP)

      USE MODA_BITBUF
      USE MODA_MGWA
      USE MODV_IM8B

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK FOR I8 INTEGERS
C  ---------------------

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84(LUNIN,MY_LUNIN,1)
         CALL MESGBC(MY_LUNIN,MESGTYP,ICOMP)
         CALL X48(MESGTYP,MESGTYP,1)
         CALL X48(ICOMP,ICOMP,1)

         IM8B=.TRUE.
         RETURN
      ENDIF

      LUNIT = ABS(LUNIN)

C  DETERMINE METHOD OF OPERATION BASED ON SIGN OF LUNIN
C   LUNIN > 0 - REWIND AND LOOK FOR FIRST DATA MESSAGE (ITYPE = 0)
C   LUNIN < 0 - LOOK AT MESSAGE CURRENLY IN MEMORY     (ITYPE = 1)
C  ---------------------------------------------------------------

      ITYPE = 0
      IF(LUNIT.NE.LUNIN) ITYPE = 1

      ICOMP   =   -1
      MESGTYP = -256

      IF(ITYPE.EQ.0) THEN

         IREC    =    0

C  CALL OPENBF SINCE FILE IS NOT OPEN TO THE C INTERFACE YET
C  ---------------------------------------------------------

         CALL OPENBF(LUNIT,'INX',LUNIT)

C  READ PAST ANY BUFR TABLES AND RETURN THE FIRST MESSAGE TYPE FOUND
C  -----------------------------------------------------------------

1        CALL RDMSGW(LUNIT,MGWA,IER)
         IF(IER.EQ.-1) GOTO 900
         IF(IER.EQ.-2) GOTO 901

         IREC = IREC + 1

         MESGTYP = IUPBS01(MGWA,'MTYP')

         IF((IDXMSG(MGWA).EQ.1).OR.(IUPBS3(MGWA,'NSUB').EQ.0)) GOTO 1

      ELSE

C  RETURN MESSAGE TYPE FOR MESSAGE CURRENTLY STORED IN MEMORY
C  ----------------------------------------------------------

         CALL STATUS(LUNIT,LUN,IL,IM)

         DO I=1,12
           MGWA(I) = MBAY(I,LUN)
         ENDDO

         MESGTYP = IUPBS01(MGWA,'MTYP')

      END IF

C  SET THE COMPRESSION SWITCH
C  --------------------------

      ICOMP = IUPBS3(MGWA,'ICMP')

      GOTO 100

C  CAN ONLY GET TO STATEMENTS 900 OR 901 WHEN ITYPE = 0
C  ----------------------------------------------------

900   IF(IREC.EQ.0) THEN
         MESGTYP = -256
         ICOMP =     -3
      ELSE
         IF(MESGTYP.GE.0) MESGTYP = -MESGTYP
         ICOMP  = -2
      ENDIF
      GOTO 100

901   MESGTYP = -256
      ICOMP =     -1

C  EXIT
C  ----

100   IF(ITYPE.EQ.0) CALL CLOSBF(LUNIT)
      RETURN
      END
