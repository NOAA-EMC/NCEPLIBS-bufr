C> @file
C> @brief Read the next uncompressed BUFR data subset into internal arrays.
C>
C> @author Woollen @date 1994-01-06

C> Unpack the next subset from the internal
C> uncompressed message buffer (array mbay in module @ref moda_bitbuf) and
C> stores the result within the internal array val(*,lun)
C> in module @ref moda_usrint.
C>
C> @param[in] LUN - integer: file ID
C> @param[out] IRET - integer: return code:
C> - 0 normal return
C> - -1 An error occurred, possibly due to a corrupt subset in the input message.
C>
C> @author Woollen @date 1994-01-06
      SUBROUTINE RDTREE(LUN,IRET)

      use modv_vars, only: bmiss

      use moda_usrint
      use moda_usrbit
      use moda_ival
      use moda_bitbuf
      use moda_tables

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
C        will be unpacked here, so a separate subsequent call to
C        subroutine readlc() will be needed to fully
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
