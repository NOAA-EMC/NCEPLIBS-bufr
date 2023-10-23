C> @file
C> @brief Check whether a string is in the string cache.
C>
C> @author Woollen @date 1994-01-06

C> Check to see if a user-specified character
C> string is in the string cache (arrays in common blocks /stcach/ and
C> /stords/). If it is not in the cache, call
C> subroutine parusr() to perform the task of
C> separating and checking the individual "pieces" (i.e., mnemonics)
C> so that it can then be added to the cache.
C>
C> The mnemonic string cache is a performance enhancing device which saves
C> time when the same mnemonic strings are repeatedly encountered within
C> an application program.
C>
C> @param[in] STR - character*(*): string of blank-separated mnemonics.
C> @param[in] LUN - integer: file ID.
C> @param[out] I1 - integer: a number greater than or equal to the number
C> of blank-separated mnemonics in STR.
C> @param[in] IO - integer: status indicator for BUFR file associated
C> with LUN:
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
