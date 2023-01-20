C> @file
C> @brief Return the next available positional index
C> for writing into the internal jump/link table in module tables,
C> and it also uses that index to store atag and atyp within,
C> respectively, the internal jump/link table arrays tag(*) and typ(*).
C>
C> ### Program History Log
C> Date | Programmer | Comments
C> -----|------------|----------
C> 1994-01-06 | J. Woollen | Original author.
C> 1998-07-08 | J. Woollen | Replaced call to cray library routine "abort" with call to new internal bufrlib routine bort().
C> 2003-11-04 | J. Ator    | Added documentation.
C> 2003-11-04 | S. Bender  | Added remarks/bufrlib routine interdependencies.
C> 2003-11-04 | D. Keyser  | maxjl increased to 16000; unified/portable for wrf; documentation; outputs more info when routine terminates abnormally.
C> 2014-12-10 | J. Ator    | Use modules instead of common blocks.
C>
C> @author Woollen @date 1994-01-06
      
C> This subroutine returns the next available positional index
C> for writing into the internal jump/link table in module tables,
C> and it also uses that index to store atag and atyp within,
C> respectively, the internal jump/link table arrays tag(*) and typ(*).
C> If there is no more room for additional entries within the internal
C> jump/link table, then an appropriate call is made to subroutine bort().
C>
C> @param[in] ATAG - character*(*): mnemonic name.
C> @param[in] ATYP - character*(*): mnemonic type.
C> @param[in] NODE - integer: next available positional index for writing
C> into the internal jump/link table.
C>
C> @author Woollen @date 1994-01-06
      SUBROUTINE INCTAB(ATAG,ATYP,NODE)

      USE MODA_TABLES

      CHARACTER*(*) ATAG,ATYP
      CHARACTER*128 BORT_STR

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      NTAB = NTAB+1
      IF(NTAB.GT.MAXTAB) GOTO 900
      TAG(NTAB) = ATAG
      TYP(NTAB) = ATYP
      NODE = NTAB

C  EXITS
C  -----

      RETURN
 900  WRITE(BORT_STR,'("BUFRLIB: INCTAB - THE NUMBER OF JUMP/LINK '//
     . 'TABLE ENTRIES EXCEEDS THE LIMIT, MAXTAB (",I7,")")') MAXTAB
      CALL BORT(BORT_STR)
      END
