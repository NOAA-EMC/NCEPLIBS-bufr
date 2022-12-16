C> @file
C> @brief Read one or more data values from a data subset in
C> internal arrays.

C> This subroutine provides a handy way to combine the functionality
C> of subroutines rdmemm(), rdmems() and ufbint() within a single
C> subroutine call.
C>
C> @author J. Woollen
C> @date 1994-01-06
C>
C> @param[in] IMSG   -- integer: Number of BUFR message to be
C>                      read into scope for further processing,
C>                      counting from the beginning of the
C>                      internal arrays in memory
C> @param[in] ISUB   -- integer: Number of data subset to be
C>                      read from the (IMSG)th BUFR message,
C>                      counting from the beginning of the message
C> @param[out] USR   -- real*8(*,*): Data values
C> @param[in] I1     -- integer: First dimension of USR as
C>                      allocated within the calling program
C> @param[in] I2     -- integer: Second dimension of USR as
C>                      allocated within the calling program
C> @param[out] IRET -- integer: Number of replications of STR that were
C>                     read from the data subset
C> @param[in] STR  -- character*(*): String of blank-separated
C>                    Table B mnemonics in one-to-one
C>                    correspondence with the number of data
C>                    values that will be read from the data
C>                    subset within the first dimension of USR (see
C>                    [DX BUFR Tables](@ref dfbftab) for further
C>                    information about Table B mnemonics)
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 1994-01-06 | J. Woollen | Original author |
C> | 1998-07-08 | J. Woollen | Replaced call to Cray library routine "ABORT" with call to new internal routine bort() |
C> | 1999-11-18 | J. Woollen | Increased MAXMEM from 4 Mb to 8 Mb |
C> | 2001-08-15 | D. Keyser  | Increased MAXMEM from 8 Mb to 16 Mb |
C> | 2004-11-15 | D. Keyser  | Increased MAXMEM from 16 Mb to 50 Mb |
C> | 2009-04-21 | J. Ator    | Use errwrt() |
C> | 2014-12-10 | J. Ator    | Use modules instead of COMMON blocks |
C> | 2022-10-04 | J. Ator    | Added 8-byte wrapper |

      RECURSIVE SUBROUTINE UFBRMS(IMSG,ISUB,USR,I1,I2,IRET,STR)

      USE MODV_IM8B

      USE MODA_MSGCWD
      USE MODA_MSGMEM

      COMMON /QUIET / IPRT

      CHARACTER*(*) STR
      CHARACTER*128 BORT_STR,ERRSTR
      CHARACTER*8   SUBSET
      REAL*8        USR(I1,I2)

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK FOR I8 INTEGERS
C  ---------------------

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84(IMSG,MY_IMSG,1)
         CALL X84(ISUB,MY_ISUB,1)
         CALL X84(I1,MY_I1,1)
         CALL X84(I2,MY_I2,1)
         CALL UFBRMS(MY_IMSG,MY_ISUB,USR,MY_I1,MY_I2,IRET,STR)
         CALL X48(IRET,IRET,1)

         IM8B=.TRUE.
         RETURN
      ENDIF

      IRET = 0
      IF(I1.LE.0) THEN
         IF(IPRT.GE.0) THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      ERRSTR = 'BUFRLIB: UFBRMS - 4th ARG. (INPUT) IS .LE. 0, ' //
     .   'SO RETURN WITH 6th ARG. (IRET) = 0; 7th ARG. (STR) ='
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT(STR)
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
         ENDIF
         GOTO 100
      ELSEIF(I2.LE.0) THEN
         IF(IPRT.GE.0) THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      ERRSTR = 'BUFRLIB: UFBRMS - 5th ARG. (INPUT) IS .LE. 0, ' //
     .   'SO RETURN WITH 6th ARG. (IRET) = 0; 7th ARG. (STR) ='
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT(STR)
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
         ENDIF
         GOTO 100
      ENDIF

C  UFBINT SUBSET #ISUB FROM MEMORY MESSAGE #IMSG
C  ---------------------------------------------

      CALL RDMEMM(IMSG,SUBSET,JDATE,IRET)
      IF(IRET.LT.0) GOTO 900
      CALL RDMEMS(ISUB,IRET)
      IF(IRET.NE.0) GOTO 901

      CALL UFBINT(MUNIT,USR,I1,I2,IRET,STR)

C  EXITS
C  -----

100   RETURN
900   IF(IMSG.GT.0)  THEN
      WRITE(BORT_STR,'("BUFRLIB: UFBRMS - REQUESTED MEMORY MESSAGE '//
     . 'NUMBER TO READ IN (",I5,") EXCEEDS THE NUMBER OF MESSAGES IN '//
     . 'MEMORY (",I5,")")') IMSG,MSGP(0)
      ELSE
      WRITE(BORT_STR,'("BUFRLIB: UFBRMS - REQUESTED MEMORY MESSAGE '//
     . 'NUMBER TO READ IN IS ZERO - THIS IS NOT VALID")')
      ENDIF
      CALL BORT(BORT_STR)
901   CALL STATUS(MUNIT,LUN,IL,IM)
      WRITE(BORT_STR,'("BUFRLIB: UFBRMS - REQ. SUBSET NUMBER TO READ '//
     . 'IN (",I3,") EXCEEDS THE NUMBER OF SUBSETS (",I3,") IN THE '//
     . 'REQ. MEMORY MESSAGE (",I5,")")') ISUB,MSUB(LUN),IMSG
      CALL BORT(BORT_STR)
      END
