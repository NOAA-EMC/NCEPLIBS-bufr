C> @file
C> @brief Read data descriptors from Section 3 of a BUFR message.

C> This subroutine returns the sequence of data descriptors
C> contained within Section 3 of a BUFR message.
C>
C> @author J. Ator
C> @date 2003-11-04
C>
C> @param[in]  MBAY   -- integer(*): BUFR message
C> @param[in] LCDS3   -- integer: Dimensioned size of CDS3;
C>                       used by the subroutine to ensure that
C>                       it doesn't overflow the CDS3 array
C> @param[out] CDS3   -- character*6(*): Data descriptor sequence
C>                       within Section 3 of MBAY
C> @param[out] NDS3   -- integer: Number of data descriptors in CDS3
C>
C> @remarks
C> - The start of the BUFR message (i.e. the string 'BUFR') must be
C>   aligned on the first 4 bytes of MBAY.
C> - This subroutine does not recursively resolve any Table D
C>   descriptors from within Section 3; rather, what is returned in
C>   CDS3 is the exact list of data descriptors as it appears within
C>   Section 3 of MBAY.
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 2003-11-04 | J. Ator | Original author |
C> | 2004-08-18 | J. Ator | Removed IFIRST check, since wrdlen() now keeps track of whether it has been called |
C> | 2005-11-29 | J. Ator | Use getlens() |
C> | 2009-03-23 | J. Ator | Added LCDS3 argument and check |
C>
      SUBROUTINE UPDS3(MBAY,LCDS3,CDS3,NDS3)

      DIMENSION MBAY(*)

      CHARACTER*6 CDS3(*), ADN30

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C     Call subroutine WRDLEN to initialize some important information
C     about the local machine, just in case subroutine OPENBF hasn't
C     been called yet.

      CALL WRDLEN

C     Skip to the beginning of Section 3.

      CALL GETLENS(MBAY,3,LEN0,LEN1,LEN2,LEN3,L4,L5)
      IPT = LEN0 + LEN1 + LEN2

C     Unpack the Section 3 descriptors.

      NDS3 = 0
      DO JJ = 8,(LEN3-1),2
         NDS3 = NDS3 + 1
         IF(NDS3.GT.LCDS3) GOTO 900
         CDS3(NDS3) = ADN30(IUPB(MBAY,IPT+JJ,16),6)
      ENDDO

      RETURN
900   CALL BORT('BUFRLIB: UPDS3 - OVERFLOW OF OUTPUT DESCRIPTOR '//
     . 'ARRAY; TRY A LARGER DIMENSION FOR THIS ARRAY')
      END
