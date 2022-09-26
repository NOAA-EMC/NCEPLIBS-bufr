C> @file
C> @brief Get information about a BUFR message

C> This subroutine examines a BUFR message and returns both the
C> message type (from Section 1) and message compression indicator
C> (from Section 3).
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
C> @author D. Keyser
C> @date 2003-11-04
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
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 2003-11-04 | D. Keyser | Original author |
C> | 2004-06-29 | D. Keyser | Added LUNIN < 0 option |
C> | 2004-08-09 | J. Ator   | Maximum message length increased from 20,000 to 50,000 bytes |
C> | 2005-11-29 | J. Ator   | Use iupbs01(), getlens(), and rdmsgw() |
C> | 2009-03-23 | J. Ator   | Use iupbs3() and idxmsg() |
C> | 2012-09-15 | J. Woollen | Convert to C language I/O interface; add openbf() and closbf() for LUNIN > 0 option |
C> | 2014-12-10 | J. Ator    | Use modules instead of COMMON blocks |
C> | 2022-08-04 | J. Woollen | Added 8-byte wrapper |

      SUBROUTINE MESGBC(LUNIN,MESGTYP,ICOMP)

      USE MODA_BITBUF
      USE MODA_MGWA
      USE MODV_IM8B

      INTEGER*8 LUNIN_8,MESGTYP_8,ICOMP_8

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK FOR I8 INTEGERS
C  ---------------------
      IF(IM8B) THEN
         IM8B=.FALSE.

         LUNIN_8=LUNIN
         CALL MESGBC_8(LUNIN_8,MESGTYP_8,ICOMP_8)
         MESGTYP=MESGTYP_8
         ICOMP=ICOMP_8

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

C> This subroutine is an internal wrapper for handling 8-byte integer
C> arguments to subroutine mesgbc().
C>
C> <p>Application programs which use 8-byte integer arguments should
C> never call this subroutine directly; instead, such programs should
C> make an initial call to subroutine setim8b() with int8b=.TRUE. and
C> then call subroutine mesgbc() directly.
C>
C> @author J. Woollen
C> @date 2022-08-04
C>
C> @param[in] LUNIN_8 -- integer*8: Absolute value is Fortran logical unit
C>                       number for BUFR file
C> @param[out] MESGTYP_8 -- integer*8: Message type
C> @param[out] ICOMP_8 -- integer*8: Message compression indicator
C>
C> <b>Program history log:</b>
C> | Date       | Programmer | Comments             |
C> | -----------|------------|----------------------|
C> | 2022-08-04 | J. Woollen | Original author      |

      SUBROUTINE MESGBC_8(LUNIN_8,MESGTYP_8,ICOMP_8)

      INTEGER*8 LUNIN_8,MESGTYP_8,ICOMP_8

      LUNIN=LUNIN_8
      CALL MESGBC(LUNIN,MESGTYP,ICOMP)
      MESGTYP_8=MESGTYP
      ICOMP_8=ICOMP

      RETURN
      END
