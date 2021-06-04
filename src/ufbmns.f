C> @file
C> @brief Read a specified data subset from internal arrays.

C> This subroutine reads a specified data subset from internal
C> arrays in memory, so that it is now in scope for processing
C> via calls to any of the [values-reading subroutines](@ref hierarchy)
C> using the Fortran logical unit number IUNIT that was returned from
C> the most recent call to subroutine ufbmem().
C>
C> <p>This subroutine does not return any information about which
C> BUFR message within the internal arrays contained the specified data
C> subset.
C>
C> @author J. Woollen
C> @date 1994-01-06
C>
C> @param[in] IREP    - integer: Number of data subset to be
C>                      read into scope for further processing,
C>                      counting from the beginning of the
C>                      internal arrays in memory
C> @param[out] SUBSET - character*8: Table A mnemonic for type of BUFR
C>                      message that was read into scope
C>                      (see [DX BUFR Tables](@ref dfbftab) for
C>                      further information about Table A mnemonics)
C> @param[out] IDATE  - integer: Date-time stored within Section 1 of
C>                      BUFR message that was read into scope,
C>                      in format of either YYMMDDHH or YYYYMMDDHH,
C>                      depending on the most
C>                      recent call to subroutine datelen()
C>
C> <b>Program history log:</b>
C> - 1994-01-06  J. Woollen -- Original author
C> - 1998-07-08  J. Woollen -- Replaced call to Cray library routine
C>                           "ABORT" with call to new internal BUFRLIB
C>                           routine "BORT"
C> - 1999-11-18  J. Woollen -- Increased MAXMEM from 4 Mb to 8 Mb
C> - 2001-08-15  D. Keyser  -- Increased MAXMEM from 8 Mb to 16 Mb
C> - 2004-11-15  D. Keyser  -- Increased MAXMEM from 16 Mb to 50 Mb
C> - 2009-03-23  J. Ator    -- Use ireadmm() instead of rdmemm();
C>                           simplify logic
C> - 2014-12-10  J. Ator    -- Use modules instead of COMMON blocks
C>
      SUBROUTINE UFBMNS(IREP,SUBSET,IDATE)

      USE MODA_MSGMEM

      CHARACTER*128 BORT_STR
      CHARACTER*8   SUBSET

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

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
