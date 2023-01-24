C> @file
C> @brief Unpack and return a character string of
c> length nchr contained within nchr bytes of ibay, starting with bit
C> (IBIT+1).
C> 
C> ### Program History Log
C> Date | Programmer | Comments |
C> -----|------------|----------|
C> 1994-01-06 | J. Woollen | original author
C> 2003-11-04 | J. Ator    | added documentation
C> 2003-11-04 | S. Bender  | added remarks/bufrlib routine interdependencies
C> 2003-11-04 | D. Keyser  | unified/portable for wrf; added history documentation
C> 2009-03-23 | J. Ator    | treat null characters as blanks; prevent overflow of chr
C> 2014-11-19 | J. Ator    | add cnvnull argument
C>
C> @author Woollen @date 1994-01-06
      
C> This subroutine unpacks and returns a character string of
c> length nchr contained within nchr bytes of ibay, starting with bit
C> (IBIT+1). On output, IBIT is updated to point to the last bit that
c> was unpacked. Note that the string to be unpacked does not
c> necessarily need to be aligned on a byte boundary within ibay.
C>
C> @note: This subroutine is the inverse of pkc().
C>
C> @param[in] NCHR - integer: number of bytes of ibay within which to
C> unpack chr (i,e, the number of characters in chr)
C> @param[in] IBAY - integer: *-word packed binary array containing packed CHR.
C> @param[in] IBIT - integer: bit pointer within ibay indicating bit after
C> which to start unpacking.
C> @param[in] CNVNULL - logical: .true. if null characters should be.
C> converted to blanks.
C> @param[out] CHR - character*(*): unpacked character string of length NCHR.
C> @param[out] IBIT - integer: bit pointer within ibay indicating last bit
C> that was unpacked.
C>
C> @author Woollen @date 1994-01-06
      SUBROUTINE UPC(CHR,NCHR,IBAY,IBIT,CNVNULL)



      COMMON /CHARAC/ IASCII,IATOE(0:255),IETOA(0:255)
      COMMON /HRDWRD/ NBYTW,NBITW,IORD(8)

      CHARACTER*(*) CHR
      CHARACTER*8   CVAL
      DIMENSION     IBAY(*),IVAL(2)
      EQUIVALENCE   (CVAL,IVAL)

      LOGICAL CNVNULL

C----------------------------------------------------------------------
C----------------------------------------------------------------------

      LB = IORD(NBYTW)
      CVAL = ' '

      NUMCHR = MIN(NCHR,LEN(CHR))
      DO I=1,NUMCHR
        CALL UPB(IVAL(1),8,IBAY,IBIT)
        IF((IVAL(1).EQ.0).AND.(CNVNULL)) THEN
          CHR(I:I) = ' '
        ELSE 
          CHR(I:I) = CVAL(LB:LB)
        ENDIF
        IF(IASCII.EQ.0) CALL IPKM(CHR(I:I),1,IATOE(IUPM(CHR(I:I),8)))
      ENDDO

      RETURN
      END
