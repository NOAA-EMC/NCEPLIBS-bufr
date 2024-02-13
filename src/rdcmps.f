C> @file
C> @brief Read the next compressed BUFR data subset into internal arrays.
C>
C> @author Woollen @date 2000-09-19

C> Uncompress and unpack the next subset
C> from the internal compressed message buffer (array mbay in module
C> @ref moda_bitbuf) and store the result within the internal
C> array val(*,lun) in module @ref moda_usrint.
C>
C> @param[in] LUN - integer: File ID.
C>
C> @author Woollen @date 2000-09-19
      SUBROUTINE RDCMPS(LUN)

      use modv_vars, only: bmiss, mxrst

      use moda_usrint
      use moda_msgcwd
      use moda_bitbuf
      use moda_tables
      use moda_rlccmn
      use moda_stcode

      CHARACTER*128 BORT_STR
      CHARACTER*8   CREF,CVAL
      EQUIVALENCE   (CVAL,RVAL)
      REAL*8        RVAL,UPS

      integer(8) :: ival,lref,ninc,lps

C-----------------------------------------------------------------------
C     Statement function to compute BUFR "missing value" for field
C     of length LBIT bits (all bits "on"):

      LPS(LBIT) = MAX(2_8**(LBIT)-1,1)
C-----------------------------------------------------------------------

C  SETUP THE SUBSET TEMPLATE
C  -------------------------

      CALL USRTPL(LUN,1,1)

C  UNCOMPRESS A SUBSET INTO THE VAL ARRAY ACCORDING TO TABLE B
C  -----------------------------------------------------------

      NSBS = NSUB(LUN)

C     Note that we are going to unpack the (NSBS)th subset from within
C     the current BUFR message.

      IBIT = MBYT(LUN)
      NRST = 0

C     Loop through each element of the subset.

      N = 0

1     DO N=N+1,NVAL(LUN)
      NODE = INV(N,LUN)
      NRFELM(N,LUN) = IGETRFEL(N,LUN)
      NBIT = IBT(NODE)
      ITYP = ITP(NODE)

C     In each of the following code blocks, the "local reference value"
C     for the element is determined first, followed by the 6-bit value
C     which indicates how many bits are used to store the increment
C     (i.e. offset) from this "local reference value".  Then, we jump
C     ahead to where this increment is stored for this particular subset,
C     unpack it, and add it to the "local reference value" to determine
C     the final uncompressed value for this element from this subset.

C     Note that, if an element has the same final uncompressed value
C     for each subset in the message, then the encoding rules for BUFR
C     compression dictate that the "local reference value" will be equal
C     to this value, the 6-bit increment length indicator will have
C     a value of zero, and the actual increments themselves will be
C     omitted from the message.

      IF(ITYP.EQ.1.OR.ITYP.EQ.2) THEN

C        This is a numeric element.

         if(nbit<=32) then
            CALL UPB(LRE4,NBIT,MBAY(1,LUN),IBIT)
            CALL UPB(LINC,   6,MBAY(1,LUN),IBIT)
            JBIT = IBIT + LINC*(NSBS-1)
            CALL UPB(NIN4,LINC,MBAY(1,LUN),JBIT)
            LREF=LRE4; NINC=NIN4
         elseif(nbit<=64) then
            CALL UP8(LREF,NBIT,MBAY(1,LUN),IBIT)
            CALL UPB(LINC,   6,MBAY(1,LUN),IBIT)
            JBIT = IBIT + LINC*(NSBS-1)
            CALL UP8(NINC,LINC,MBAY(1,LUN),JBIT)
         endif

         IF(NINC.EQ.LPS(LINC)) THEN
            IVAL = LPS(NBIT)
         ELSE
            IVAL = LREF+NINC
         ENDIF
         IF(ITYP.EQ.1) THEN
            NBMP=INT(IVAL)
            CALL USRTPL(LUN,N,NBMP)
            if (iscodes(lun) .ne. 0) return
            GOTO 1
         ENDIF
         IF(IVAL.LT.LPS(NBIT)) VAL(N,LUN) = UPS(IVAL,NODE)
         CALL STRBTM(N,LUN)
         IBIT = IBIT + LINC*MSUB(LUN)
      ELSEIF(ITYP.EQ.3) THEN

C        This is a character element.  If there are more than 8
C        characters, then only the first 8 will be unpacked by this
C        routine, and a separate subsequent call to
C        subroutine readlc() will be required to unpack the remainder of
C        the string.  In this case, pointers will be saved within
C        COMMON /RLCCMN/ for later use within readlc().

C        Unpack the local reference value.

         LELM = NBIT/8
         NCHR = MIN(8,LELM)
         IBSV = IBIT
         CREF = ' '
         CALL UPC(CREF,NCHR,MBAY(1,LUN),IBIT,.TRUE.)
         IF(LELM.GT.8) THEN
            IBIT = IBIT + (LELM-8)*8
            NRST = NRST + 1
            IF(NRST.GT.MXRST) GOTO 900
            CRTAG(NRST) = TAG(NODE)
         ENDIF

C        Unpack the increment length indicator.  For character elements,
C        this length is in bytes rather than bits.

         CALL UPB(LINC,   6,MBAY(1,LUN),IBIT)
         IF(LINC.EQ.0) THEN
            IF(LELM.GT.8) THEN
               IRNCH(NRST) = LELM
               IRBIT(NRST) = IBSV
            ENDIF
            CVAL = CREF
         ELSE
            JBIT = IBIT + LINC*(NSBS-1)*8
            IF(LELM.GT.8) THEN
               IRNCH(NRST) = LINC
               IRBIT(NRST) = JBIT
            ENDIF
            NCHR = MIN(8,LINC)
            CVAL = ' '
            CALL UPC(CVAL,NCHR,MBAY(1,LUN),JBIT,.TRUE.)
         ENDIF
         IF (LELM.LE.8 .AND. ICBFMS(CVAL,NCHR).NE.0) THEN
            VAL(N,LUN) = BMISS
         ELSE
            VAL(N,LUN) = RVAL
         ENDIF
         IBIT = IBIT + 8*LINC*MSUB(LUN)
      ENDIF
      ENDDO

      RETURN
900   WRITE(BORT_STR,'("BUFRLIB: RDCMPS - NUMBER OF LONG CHARACTER ' //
     .   'STRINGS EXCEEDS THE LIMIT (",I4,")")') MXRST
      CALL BORT(BORT_STR)
      END
