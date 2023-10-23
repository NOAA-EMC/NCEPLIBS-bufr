C> @file
C> @brief Pack a BUFR data subset.
C>
C> @author J. Woollen @date 1994-01-06

C> Convert user numbers from the val array into scaled
C> integers and then pack them into bit strings in the ibay array.
C>
C> @param[in] lun - integer: file ID.
C>
C> @author J. Woollen @date 1994-01-06

      SUBROUTINE WRTREE(LUN)

      USE MODA_USRINT
      USE MODA_IVAL
      USE MODA_UFBCPL
      USE MODA_BITBUF
      USE MODA_TABLES

      CHARACTER*120 LSTR
      CHARACTER*8   CVAL
      EQUIVALENCE   (CVAL,RVAL)
      integer(8)    ipks
      REAL*8        RVAL

C-----------------------------------------------------------------------

C  CONVERT USER NUMBERS INTO SCALED INTEGERS
C  -----------------------------------------

      DO N=1,NVAL(LUN)
      NODE = INV(N,LUN)
      IF(ITP(NODE).EQ.1) THEN
         IVAL(N) = NINT(VAL(N,LUN))
      ELSEIF(TYP(NODE).EQ.'NUM') THEN
         IF(IBFMS(VAL(N,LUN)).EQ.0) THEN
            IVAL(N) = IPKS(VAL(N,LUN),NODE)
         ELSE
            IVAL(N) = -1
         ENDIF
      ENDIF
      ENDDO

C  PACK THE USER ARRAY INTO THE SUBSET BUFFER
C  ------------------------------------------

      IBIT = 16

      DO N=1,NVAL(LUN)
      NODE = INV(N,LUN)
      IF(ITP(NODE).LT.3) THEN

C        The value to be packed is numeric.

         CALL PKB8(IVAL(N),IBT(NODE),IBAY,IBIT)
      ELSE

C        The value to be packed is a character string.

         NCR=IBT(NODE)/8
         IF ( NCR.GT.8 .AND. LUNCPY(LUN).NE.0 ) THEN

C           The string is longer than 8 characters and there was a
C           preceeding call to UFBCPY involving this output unit, so
C           read the long string with READLC and write it into the
C           output buffer using PKC.

            CALL READLC(LUNCPY(LUN),LSTR,TAG(NODE))
            CALL PKC(LSTR,NCR,IBAY,IBIT)
         ELSE
            RVAL = VAL(N,LUN)
            IF(IBFMS(RVAL).NE.0) THEN

C              The value is "missing", so set all bits to 1 before
C              packing the field as a character string.

               NUMCHR = MIN(NCR,LEN(LSTR))
               DO JJ = 1, NUMCHR
                  CALL IPKM(LSTR(JJ:JJ),1,255)
               ENDDO
               CALL PKC(LSTR,NUMCHR,IBAY,IBIT)
            ELSE

C              The value is not "missing", so pack the equivalenced
C              character string.  Note that a maximum of 8 characters
C              will be packed here, so a separate subsequent call to
C              subroutine writlc() will be needed to
C              fully encode any string longer than 8 characters.

               CALL PKC(CVAL,NCR,IBAY,IBIT)
            ENDIF
         ENDIF

      ENDIF
      ENDDO

C  RESET UFBCPY FILE POINTER
C  -------------------------

      LUNCPY(LUN)=0

      RETURN
      END
