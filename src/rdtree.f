C> @file
C> @brief Read the next uncompressed BUFR data subset into internal arrays.
C>
C> ### Program History Log
C> Date | Programmer | Comments
C> -----|------------|----------
C> 1994-01-06 | J. Woollen | original author
C> 1998-10-27 | J. Woollen | modified to correct problems caused by in- lining code with fpp directives
C> 1999-11-18 | J. Woollen | the number of bufr files which can be opened at one time increased from 10 to 32.
C> 2000-09-19 | J. Woollen | maximum message length increased from 10,000 to 20,000 bytes
C> 2003-11-04 | J. Woollen | fixed a bug which could only occur when the last element in a subset is a character
C> 2003-11-04 | S. Bender  | added remarks/bufrlib routine interdependencies
C> 2003-11-04 | D. Keyser  | maxjl increased to 16000; unified/portable for wrf; added documentation (including history)
C> 2004-08-09 | J. Ator    | maximum message length increased from 20,000 to 50,000 bytes
C> 2007-01-19 | J. Ator    | prevent overflow of cval for strings longer than 8 characters
C> 2012-03-02 | J. Ator    | use function ups
C> 2012-06-04 | J. Ator    | set decoded real*8 value to "missing" when corresponding character field has all bits set to 1
C> 2014-12-10 | J. Ator    | use modules instead of common blocks
C> 2016-11-09 | J. Ator    | added iret argument and check for possibly corrupt subsets
C> 2022-05-06 | J. Woollen | replace upbb with upb8 for 8byte integers
C>
C> @author Woollen @date 1994-01-06

C> This subroutine unpacks the next subset from the internal
c> uncompressed message buffer (array mbay in module bitbuf) and
c> stores the unpacked subset within the internal array val(*,lun)
c> in module usrint.
C>
C> @param[in] LUN - integer: I/O stream index into internal memory arrays.
C>
C> @param[out] IRET - integer: return code:
C> - 0 normal return
C> - -1 An error occurred, possibly due to a corrupt subset in the input message.
C>
C> @author Woollen @date 1994-01-06
      SUBROUTINE RDTREE(LUN,IRET)

      USE MODV_BMISS
      USE MODA_USRINT
      USE MODA_USRBIT
      USE MODA_IVAL
      USE MODA_BITBUF
      USE MODA_TABLES

      CHARACTER*8  CVAL
      EQUIVALENCE  (CVAL,RVAL)
      REAL*8       RVAL,UPS

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      IRET = 0

C  CYCLE THROUGH A SUBSET SETTING UP THE TEMPLATE
C  ----------------------------------------------

      MBIT(1) = IBIT
      NBIT(1) = 0
      CALL RCSTPL(LUN,IER)
      IF(IER.NE.0) THEN
        IRET = -1
        RETURN
      ENDIF

C  UNPACK A SUBSET INTO THE USER ARRAY IVAL
C  ----------------------------------------

      DO N=1,NVAL(LUN)
      CALL UPB8(IVAL(N),NBIT(N),MBIT(N),MBAY(1,LUN))
      ENDDO

C  LOOP THROUGH EACH ELEMENT OF THE SUBSET, CONVERTING THE UNPACKED
C  VALUES TO THE PROPER TYPES
C  ----------------------------------------------------------------

      DO N=1,NVAL(LUN)
      NODE = INV(N,LUN)
      IF(ITP(NODE).EQ.1) THEN

C        The unpacked value is a delayed descriptor replication factor.

         VAL(N,LUN) = IVAL(N)
      ELSEIF(ITP(NODE).EQ.2) THEN

C        The unpacked value is a real.

         IF (IVAL(N).LT.2_8**ibt(node)-1) THEN
            VAL(N,LUN) = UPS(IVAL(N),NODE)
         ELSE
            VAL(N,LUN) = BMISS
         ENDIF
      ELSEIF(ITP(NODE).EQ.3) THEN

C        The value is a character string, so unpack it using an
C        equivalenced REAL*8 value.  Note that a maximum of 8 characters
C        will be unpacked here, so a separate subsequent call to BUFR
C        archive library subroutine READLC will be needed to fully
C        unpack any string longer than 8 characters.

         CVAL = ' '
         KBIT = MBIT(N)
         NBT = MIN(8,NBIT(N)/8)
         CALL UPC(CVAL,NBT,MBAY(1,LUN),KBIT,.TRUE.)
         IF (NBIT(N).LE.64 .AND. ICBFMS(CVAL,NBT).NE.0) THEN
            VAL(N,LUN) = BMISS
         ELSE
            VAL(N,LUN) = RVAL
         ENDIF
      ENDIF
      ENDDO

      IBIT = NBIT(NVAL(LUN))+MBIT(NVAL(LUN))

      RETURN
      END
