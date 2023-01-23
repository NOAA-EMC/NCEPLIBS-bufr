C> @file
C> @brief Packs a BUFR subset from array form into bit string output format.
C>
C> ### Program History Log
C> Date | Programmer | Comments |
C> -----|------------|----------|
C> 1994-01-06 | J. Woollen  |  original author 
C> 1998-07-08 | J. Woollen  |  corrected some minor errors 
C> 1999-11-18 | J. Woollen  |  the number of bufr files which can be opened at one time increased from 10 to 32 
C> 2000-09-19 | J. Woollen  |  maximum message length increased from 10k TO 20k bytes 
C> 2003-11-04 | S. Bender   |  added remarks/bufrlib routine 
C> 2003-11-04 | D. Keyser   |  maxjl (maximum number of jump/link entries) increased from 15K to 16K 
C> 2004-03-10 | J. Woollen  |  converted packing function 'pks' to real*8 
C> 2004-08-09 | J. Ator     |  maximum message length increased from 20K TO 50K 
C> 2007-01-19 | J. Ator     |  prevent overflow of cval for strings longer than 8 characters; use function ibfms 
C> 2009-08-03 | J. Woollen  |  added capability to copy long strings via ufbcpy using file pointer stored in new common ufbcpl 
C> 2012-03-02 | J. Ator     |  use ipks to handle 2-03 operator cases 
C> 2012-06-04 | J. Ator     |  ensure "missing" character fields are properly encoded with all bits set to 1 
C> 2014-12-10 | J. Ator     |  use modules instead of common blocks 
C> 2022-05-06 | J. Woollen  |  replace pkb with pkb8 for 8byte integers 
C>
C> @author J. Woollen @date 1994-01-06

C> This subroutine converts user numbers in the val array into scaled integers
C> and packs them into bit strings in the ibit subset output buffer.
C>
C> @param[in] lun -- integer: i/o stream index into internal memory arrays
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
         IVAL(N) = VAL(N,LUN)
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

C	 The value to be packed is numeric.

         CALL PKB8(IVAL(N),IBT(NODE),IBAY,IBIT)
      ELSE

C	 The value to be packed is a character string.

         NCR=IBT(NODE)/8
         IF ( NCR.GT.8 .AND. LUNCPY(LUN).NE.0 ) THEN

C	    The string is longer than 8 characters and there was a
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
C              BUFR archive library subroutine WRITLC will be needed to
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
