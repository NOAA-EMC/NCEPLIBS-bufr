C> @file
C> @brief Check to see if a user-specified character
c> string is in the string cache.
C>
C> ### Program History Log
C> Date | Programmer | Comments
C> -----|------------|---------
C> 1994-01-06 | J. Woollen | original author
C> 1998-04-02 | J. Woollen | modified to enlarge the cache from 50 elements to 1000, maximum; optimization of the cache search algorithm in support of a bigger cache
C> 1998-07-08 | J. Woollen | replaced call to cray library routine "abort" with call to new internal bufrlib routine "bort"; corrected some minor errors
C> 1999-11-18 | J. Woollen | the number of bufr files which can be opened at one time increased from 10 to 32
C> 2003-11-04 | S. Bender  | added remarks/bufrlib routine interdependencies
C> 2003-11-04 | D. Keyser  | unified/portable for wrf; documentation; more info; changed call to bort2()
C> 2014-12-10 | J. Ator    | use modules instead of common blocks
C>
C> @author Woollen @date 1994-01-06
      
C> This subroutine checks to see if a user-specified character
c> string is in the string cache (arrays in common blocks /stcach/ and
c> /stords/). If it is not in the cache, it must call the bufr
c> archive library parsing subroutine parusr to perform the task of
c> separating and checking the individual "pieces" (i.e., mnemonics)
c> so that it can then be added to the cache. If it is already in the
c> cache, then this extra work does not need to be performed. The
c> mnemonic string cache is a performance enhancing device which saves
c> time when the same mnemonic strings are encountered in a user
c> program, over and over again (the typical scenario).
C>
C> @param[in] STR - character*(*): string of blank-separated mnemonics.
C> @param[in] LUN - integer: i/o stream index into internal memory arrays.
C> @param[out] I1 - integer: a number greater than or equal to the number
C> of blank-separated mnemonics in str.
C> @param[out] IO - integer: status indicator for bufr file associated
C> with lun:
C> - 0 input file
C> - 1 output file
C>
C> @author Woollen @date 1994-01-06
      SUBROUTINE STRING(STR,LUN,I1,IO)

      USE MODV_MXS
      USE MODA_MSGCWD

      PARAMETER (JCONS=52)

      COMMON /STCACH/ MSTR,NSTR,LSTR,LUX(MXS,2),USR(MXS),ICON(JCONS,MXS)
      COMMON /USRSTR/ JCON(JCONS)
      COMMON /STORDS/ IORD(MXS),IORX(MXS)

      CHARACTER*(*) STR
      CHARACTER*128 BORT_STR1,BORT_STR2
      CHARACTER*80  USR,UST

C----------------------------------------------------------------------
C----------------------------------------------------------------------

      NXT = 0
      UST = STR
      IND = INODE(LUN)
      IF(LEN(STR).GT.80) GOTO 900

C     Note that LSTR, MSTR and NSTR were initialized via a prior call to
C     subroutine STRCLN, which itself was called by subroutine MAKESTAB.

C  SEE IF STRING IS IN THE CACHE
C  -----------------------------

      DO N=1,NSTR
      IF(LUX(IORD(N),2).EQ.IND) THEN
         IORX(NXT+1) = IORD(N)
         NXT = NXT+1
      ENDIF
      ENDDO
      DO N=1,NXT
      IF(UST.EQ.USR(IORX(N)))GOTO1
      ENDDO
      GOTO2

C  IF IT IS IN THE CACHE, COPY PARAMETERS FROM THE CACHE
C  -----------------------------------------------------

1     DO J=1,JCONS
      JCON(J) = ICON(J,IORX(N))
      ENDDO
      GOTO 100

C  IF IT IS NOT IN THE CACHE, PARSE IT AND PUT IT THERE
C  ----------------------------------------------------

2     CALL PARUSR(STR,LUN,I1,IO)
      LSTR = MAX(MOD(LSTR+1,MSTR+1),1)
      NSTR = MIN(NSTR+1,MSTR)
c  .... File
      LUX(LSTR,1) = LUN
c  .... Table A entry
      LUX(LSTR,2) = IND
      USR(LSTR) = STR
      DO J=1,JCONS
      ICON(J,LSTR) = JCON(J)
      ENDDO

C  REARRANGE THE CACHE ORDER AFTER AN UPDATE
C  -----------------------------------------

      DO N=NSTR,2,-1
      IORD(N) = IORD(N-1)
      ENDDO
      IORD(1) = LSTR

100   IF(JCON(1).GT.I1) GOTO 901

C  EXITS
C  -----

      RETURN
900   WRITE(BORT_STR1,'("BUFRLIB: STRING - INPUT STRING (",A,") HAS")')
     . STR
      WRITE(BORT_STR2,'(18X,"LENGTH (",I4,"), > LIMIT OF 80 CHAR.")')
     .  LEN(STR)
      CALL BORT2(BORT_STR1,BORT_STR2)
901   WRITE(BORT_STR1,'("BUFRLIB: STRING - INPUT STRING (",A,")")') STR
      WRITE(BORT_STR2,'(18X,"HAS",I5," STORE NODES (MNEMONICS) - THE '//
     . 'LIMIT (THIRD INPUT ARGUMENT) IS",I5)') JCON(1),I1
      CALL BORT2(BORT_STR1,BORT_STR2)
      END
