C> @file
C> @brief Read a specified data subset from internal arrays.
C>
C> @author J. Woollen @date 1994-01-06

C> This subroutine reads a specified data subset from internal
C> arrays in memory, so that it is now in scope for processing
C> via calls to any of the [values-reading subroutines](@ref hierarchy)
C> using the Fortran logical unit number IUNIT that was returned from
C> the most recent call to subroutine ufbmem().
C>
C> This subroutine does not return any information about which
C> BUFR message within the internal arrays contained the specified data
C> subset.
C>
C> @param[in] IREP   -- integer: Number of data subset to be
C>                      read into scope for further processing,
C>                      counting from the beginning of the
C>                      internal arrays in memory
C> @param[out] SUBSET -- character*8: Table A mnemonic for type of BUFR
C>                       message that was read into scope
C>                       (see [DX BUFR Tables](@ref dfbftab) for
C>                       further information about Table A mnemonics)
C> @param[out] IDATE -- integer: Date-time stored within Section 1 of
C>                      BUFR message that was read into scope,
C>                      in format of either YYMMDDHH or YYYYMMDDHH,
C>                      depending on the most
C>                      recent call to subroutine datelen()
C>
C> @author J. Woollen @date 1994-01-06
      RECURSIVE SUBROUTINE UFBMNS(IREP,SUBSET,IDATE)

      USE MODV_IM8B

      USE MODA_MSGMEM

      CHARACTER*128 BORT_STR
      CHARACTER*8   SUBSET

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK FOR I8 INTEGERS
C  ---------------------

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84(IREP,MY_IREP,1)
         CALL UFBMNS(MY_IREP,SUBSET,IDATE)
         CALL X48(IDATE,IDATE,1)

         IM8B=.TRUE.
         RETURN
      ENDIF

      JREP = 0
      IMSG = 1

C  READ SUBSET #ISUB FROM MEMORY MESSAGE #IMSG
C  -------------------------------------------

      DO WHILE(IREADMM(IMSG,SUBSET,IDATE).EQ.0)
        IF(JREP+NMSUB(MUNIT).GE.IREP) THEN
           CALL RDMEMS(IREP-JREP,IRET)
           GOTO 100
        ENDIF
        JREP = JREP+NMSUB(MUNIT)
      ENDDO
      GOTO 900

C  EXITS
C  -----

100   RETURN
900   WRITE(BORT_STR,'("BUFRLIB: UFBMNS - REQ. SUBSET NO. TO READ IN '//
     . '(",I5,") EXCEEDS TOTAL NO. OF SUBSETS IN THE COLLECTION OF '//
     . 'MEMORY MESSAGES (",I5,")")') IREP,JREP
      CALL BORT(BORT_STR)
      END
