C> @file
C> @brief Jump forwards or backwards to a specified data subset within
C> a BUFR file

C> This subroutine repositions the file pointer to the beginning of a
C> specified data subset within a specified message of a BUFR file,
C> then reads that data subset into internal arrays so that it can be
C> further processed via subsequent calls to any of the
C> [values-reading subroutines](@ref hierarchy).
C>
C> The specified data subset may be before or after the current location
C> of the file pointer within the BUFR file.
C>
C> @author J. Woollen
C> @date 1995-11-22
C>
C> @param[in] LUNIT  - integer: Fortran logical unit number for BUFR file
C> @param[in] IREC   - integer: Ordinal number of message to be read,
C>                     counting from the beginning of the BUFR file, but
C>                     not counting any messages which contain DX BUFR
C>                     tables information
C> @param[in] ISUB   - integer: Ordinal number of data subset to be
C>                     read from (IREC)th message, counting from the
C>                     beginning of the message
C> @param[out] SUBSET - character*8: Table A mnemonic for type of BUFR
C>                      message that was read
C>                      (see [DX BUFR Tables](@ref dfbftab)
C>                      for further information about Table A mnemonics)
C> @param[out] JDATE  - integer: Date-time stored within Section 1 of
C>                      BUFR message that was read, in format of either
C>                      YYMMDDHH or YYYYMMDDHH, depending on the most
C>                      recent call to subroutine datelen()
C>
C> @remarks
C> - Logical unit LUNIT should have already been opened for input
C> operations via a previous call to subroutine openbf().
C> - The value specified for IREC should <b>not</b> include any messages
C> which contain DX BUFR tables information.
C>
C> <b>Program history log:</b>
C> - 1995-11-22  J. Woollen -- Original author
C> - 2005-03-04  D. Keyser  -- Added documentation
C> - 2006-04-14  J. Ator    -- Remove unnecessary MOIN initialization
C> - 2009-03-23  J. Ator    -- Modified to handle embedded BUFR table
C>                             (dictionary) messages
C> - 2014-12-10  J. Ator    -- Use modules instead of COMMON blocks
C> - 2021-10-08  J. Ator    -- Use readsb() to read all subsets from
C>                             IREC(th) message
C>
      SUBROUTINE UFBPOS(LUNIT,IREC,ISUB,SUBSET,JDATE)

      USE MODA_MSGCWD
      USE MODA_BITBUF

      CHARACTER*128 BORT_STR
      CHARACTER*8   SUBSET
 
C-----------------------------------------------------------------------
C----------------------------------------------------------------------

C  MAKE SURE A FILE IS OPEN FOR INPUT
C  ----------------------------------

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IL.GT.0) GOTO 901

      IF(IREC.LE.0)  GOTO 902
      IF(ISUB.LE.0)  GOTO 903

C  SEE WHERE POINTERS ARE CURRENTLY LOCATED
C  ----------------------------------------

      CALL UFBCNT(LUNIT,JREC,JSUB)
 
C  REWIND FILE IF REQUESTED POINTERS ARE BEHIND CURRENT POINTERS
C  -------------------------------------------------------------
 
      IF(IREC.LT.JREC .OR. (IREC.EQ.JREC.AND.ISUB.LT.JSUB)) THEN
         CALL CEWIND(LUN)
         NMSG(LUN) = 0
         NSUB(LUN) = 0
         CALL UFBCNT(LUNIT,JREC,JSUB)
      ENDIF

C  READ SUBSET #ISUB FROM MESSAGE #IREC FROM FILE
C  ----------------------------------------------

      DO WHILE (IREC.GT.JREC)
         CALL READMG(LUNIT,SUBSET,JDATE,IRET)
         IF(IRET.LT.0) GOTO 904
         CALL UFBCNT(LUNIT,JREC,JSUB)
      ENDDO

      DO WHILE (ISUB.GT.JSUB)
         CALL READSB(LUNIT,IRET)
         IF(IRET.NE.0) GOTO 905
         CALL UFBCNT(LUNIT,JREC,JSUB)
      ENDDO

C  EXITS
C  -----

      RETURN
900   CALL BORT('BUFRLIB: UFBPOS - INPUT BUFR FILE IS CLOSED, IT MUST'//
     . ' BE OPEN FOR INPUT')
901   CALL BORT('BUFRLIB: UFBPOS - INPUT BUFR FILE IS OPEN FOR OUTPUT'//
     . ', IT MUST BE OPEN FOR INPUT')
902   WRITE(BORT_STR,'("BUFRLIB: UFBPOS - REQUESTED MESSAGE NUMBER '//
     . 'TO READ IN (",I5,") IS NOT VALID")') IREC
      CALL BORT(BORT_STR)
903   WRITE(BORT_STR,'("BUFRLIB: UFBPOS - REQUESTED SUBSET NUMBER '//
     . 'TO READ IN (",I5,") IS NOT VALID")') ISUB
      CALL BORT(BORT_STR)
904   WRITE(BORT_STR,'("BUFRLIB: UFBPOS - REQUESTED MESSAGE NUMBER '//
     . 'TO READ IN (",I5,") EXCEEDS THE NUMBER OF MESSAGES IN THE '//
     . 'FILE (",I5,")")') IREC,JREC
      CALL BORT(BORT_STR)
905   WRITE(BORT_STR,'("BUFRLIB: UFBPOS - REQ. SUBSET NUMBER TO READ'//
     . ' IN (",I3,") EXCEEDS THE NUMBER OF SUBSETS (",I3,") IN THE '//
     . 'REQ. MESSAGE (",I5,")")') ISUB,KSUB,IREC
      CALL BORT(BORT_STR)
      END
