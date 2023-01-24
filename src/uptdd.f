C> @file
C> @brief this subroutine returns the fxy value of an element in a sequence 
C>
C> ### Program History Log
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> 1994-01-06 | J. Woollen  |  original author
C> 1995-06-28 | J. Woollen  |  increased the size of internal bufr table arrays 
C> 1998-07-08 | J. Woollen  |  replaced call to cray library routine "abort" with bufrlib routine "bort"
C> 1999-11-18 | J. Woollen  |  the number of bufr files which can be opened at one time increased from 10 to 32
C> 2003-11-04 | J. Ator     |  added documentation
C> 2003-11-04 | S. Bender   |  added remarks/bufrlib routine interdependencies
C> 2003-11-04 | D. Keyser   |  unified/portable for wrf; added history documentation; outputs more complete diagnostic info 
C> 2014-12-10 | J. Ator     |  use modules instead of common blocks
C>
C> @author woollen @date 1994-01-06

C> This subroutine returns the bit-wise representation of the fxy value corresponding to, sequentially, 
C> a particular (ient'th) C>   "child" mnemonic of a table d sequence ("parent") mnemonic.
C>
C> @remark The interpretation of the return value iret depends upon the input value ient, as follows:
C> ient=0 - iret = a count of the total number of child mnemonics within tabd(id,lun)
C> ient>0 - iret = the bit-wise representation of the fxy value corresponding to the ient'th child mnemonic of tabd(id,lun)
C>
C> @param[in] id - integer: positional index of parent mnemonic within internal bufr table d array tabd
C> @param[in] lun - integer: i/o stream index into internal memory arrays
C> @param[in] ient - integer: ordinal indicator of child mnemonic to return from within tabd(id,lun) sequence:
C> @param[out] iret - integer: return value (see remarks)
C>
C> @author woollen @date 1994-01-06


      SUBROUTINE UPTDD(ID,LUN,IENT,IRET)

      USE MODA_TABABD

      COMMON /DXTAB / MAXDX,IDXV,NXSTR(10),LDXA(10),LDXB(10),LDXD(10),
     .                LD30(10),DXSTR(10)

      CHARACTER*128 BORT_STR
      CHARACTER*56  DXSTR

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      LDD = LDXD(IDXV+1)+1

C  CHECK IF IENT IS IN BOUNDS
C  --------------------------

      NDSC = IUPM(TABD(ID,LUN)(LDD:LDD),8)

      IF(IENT.EQ.0) THEN
         IRET = NDSC
         GOTO 100
      ELSEIF(IENT.LT.0 .OR. IENT.GT.NDSC) THEN
         GOTO 900
      ENDIF

C  RETURN THE DESCRIPTOR INDICATED BY IENT
C  ---------------------------------------

      IDSC = LDD+1 + (IENT-1)*2
      IRET = IUPM(TABD(ID,LUN)(IDSC:IDSC),16)

C  EXITS
C  -----

100   RETURN
900   WRITE(BORT_STR,'("BUFRLIB: UPTDD - VALUE OF THIRD ARGUMENT IENT'//
     . ' (INPUT) IS OUT OF RANGE (IENT =",I4,")")') IENT
      CALL BORT(BORT_STR)
      END
