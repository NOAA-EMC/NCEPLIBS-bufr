C> @file
C> @brief Read the next data subset from a BUFR file that was
C> previously opened for reading.
C>
C> @author J. Woollen @date 1994-01-06

C> This subroutine provides a handy way to combine the functionality
C> of subroutines readmg() and readsb() within a single subroutine
C> call.
C>
C> Logical unit LUNIT should have already been opened for
C> input operations via a previous call to subroutine openbf().
C> But once that is done, the application program can immediately call
C> this subroutine to read each new data subset from the
C> associated BUFR file, and the subroutine will automatically open
C> and close each new BUFR message internally as needed, so that
C> subsequent calls can immediately be made to any of the various
C> [values-reading subroutines](@ref hierarchy).
C>
C> @param[in] LUNIT   - integer: Fortran logical unit number for
C>                      BUFR file
C> @param[out] SUBSET - character*8: Table A mnemonic for type of
C>                      data subset that was read
C>                      (see [DX BUFR Tables](@ref dfbftab)
C>                      for further information about Table A mnemonics)
C> @param[out] JDATE  - integer: Date-time stored within Section 1 of
C>                      BUFR message containing data subset that
C>                      was read, in format of either
C>                      YYMMDDHH or YYYYMMDDHH, depending on the most
C>                      recent call to subroutine datelen()
C> @param[out] IRET   - integer: return code
C>                         - 0 = new BUFR data subset was successfully
C>                               read into internal arrays
C>                         - -1 = there are no more BUFR data subsets
C>                                in the file connected to logical unit
C>                                LUNIT
C>
C> @author J. Woollen @date 1994-01-06
      RECURSIVE SUBROUTINE READNS(LUNIT,SUBSET,JDATE,IRET)

      USE MODA_MSGCWD
      USE MODA_TABLES
      USE MODV_IM8B

      CHARACTER*8  SUBSET

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK FOR I8 INTEGERS
C  ---------------------

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84(LUNIT,MY_LUNIT,1)
         CALL READNS(MY_LUNIT,SUBSET,JDATE,IRET)
         CALL X48(JDATE,JDATE,1)
         CALL X48(IRET,IRET,1)

         IM8B=.TRUE.
         RETURN
      ENDIF

C  REFRESH THE SUBSET AND JDATE PARAMETERS
C  ---------------------------------------

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IL.GT.0) GOTO 901
      IF(INODE(LUN).EQ.0) THEN
        SUBSET = '        '
      ELSE
        SUBSET = TAG(INODE(LUN))(1:8)
      ENDIF
      JDATE  = IDATE(LUN)

C  READ THE NEXT SUBSET IN THE BUFR FILE
C  -------------------------------------

1     CALL READSB(LUNIT,IRET)
      IF(IRET.NE.0) THEN
         CALL READMG(LUNIT,SUBSET,JDATE,IRET)
         IF(IRET.EQ.0) GOTO 1
      ENDIF

C  EXITS
C  -----

      RETURN
900   CALL BORT('BUFRLIB: READNS - INPUT BUFR FILE IS CLOSED, IT MUST'//
     . ' BE OPEN FOR INPUT')
901   CALL BORT('BUFRLIB: READNS - INPUT BUFR FILE IS OPEN FOR OUTPUT'//
     . ', IT MUST BE OPEN FOR INPUT')
      END
