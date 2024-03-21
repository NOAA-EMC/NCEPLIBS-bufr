C> @file
C> @brief Get the next available positional index
C> for writing into the internal jump/link table.
C>
C> @author Woollen @date 1994-01-06

C> Return the next available positional index
C> for writing into the internal jump/link table in module @ref moda_tables,
C> and also use that index to store ATAG and ATYP within,
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

      use modv_vars, only: maxjl

      use moda_tables

      CHARACTER*(*) ATAG,ATYP
      CHARACTER*128 BORT_STR

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      NTAB = NTAB+1
      IF(NTAB.GT.MAXJL) GOTO 900
      TAG(NTAB) = ATAG
      TYP(NTAB) = ATYP
      NODE = NTAB

C  EXITS
C  -----

      RETURN
 900  WRITE(BORT_STR,'("BUFRLIB: INCTAB - THE NUMBER OF JUMP/LINK '//
     . 'TABLE ENTRIES EXCEEDS THE LIMIT, MAXJL (",I7,")")') MAXJL
      CALL BORT(BORT_STR)
      END
