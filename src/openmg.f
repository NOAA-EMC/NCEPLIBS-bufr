C> @file
C> @brief Open a new message for output in a BUFR file that was
C> previously opened for writing.
C>
C> @author J. Woollen @date 1994-01-06

C> This subroutine opens and initializes a new BUFR message within
C> internal arrays, for eventual output to logical unit LUNIT.
C>
C> @param[in] LUNIT -- integer: Fortran logical unit number for BUFR
C>                     file
C> @param[in] SUBSET -- character*(*): Table A mnemonic for type of
C>                      BUFR message to be opened
C>                      (see [DX BUFR Tables](@ref dfbftab) for
C>                      further information about Table A mnemonics)
C> @param[in] JDATE -- integer: Date-time to be stored within Section 1
C>                     of BUFR message being opened, in format of either
C>                     YYMMDDHH or YYYYMMDDHH
C>
C> Logical unit LUNIT should have already been opened for output
C> operations via a previous call to subroutine openbf().
C>
C> This subroutine is similar to subroutine openmb(), except that it
C> will always open a new message for output, regardless of the values
C> of SUBSET and JDATE.  Any existing message within the internal
C> arrays will be automatically flushed and written to logical unit LUNIT
C> via an internal call to subroutine closmg().
C>
C> @author J. Woollen @date 1994-01-06
      RECURSIVE SUBROUTINE OPENMG(LUNIT,SUBSET,JDATE)

      USE MODA_MSGCWD
      USE MODV_IM8B

      CHARACTER*(*) SUBSET

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK FOR I8 INTEGERS
C  ---------------------

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84(LUNIT,MY_LUNIT,1)
         CALL X84(JDATE,MY_JDATE,1)
         CALL OPENMG(MY_LUNIT,SUBSET,MY_JDATE)

         IM8B=.TRUE.
         RETURN
      ENDIF

C  CHECK THE FILE STATUS
C  ---------------------

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IL.LT.0) GOTO 901
      IF(IM.NE.0) CALL CLOSMG(LUNIT)
      CALL WTSTAT(LUNIT,LUN,IL, 1)

C  GET SOME SUBSET PARTICULARS
C  ---------------------------

c  .... Given SUBSET, returns MTYP,MSTB,INOD
      CALL NEMTBA(LUN,SUBSET,MTYP,MSTB,INOD)
c  .... Set pos. index for new Tbl A mnem.
      INODE(LUN) = INOD
c  .... Set date for new message
      IDATE(LUN) = I4DY(JDATE)

C  INITIALIZE THE OPEN MESSAGE
C  ---------------------------

      CALL MSGINI(LUN)
      CALL USRTPL(LUN,1,1)

C  EXITS
C  -----

      RETURN
900   CALL BORT('BUFRLIB: OPENMG - OUTPUT BUFR FILE IS CLOSED, IT '//
     . 'MUST BE OPEN FOR OUTPUT')
901   CALL BORT('BUFRLIB: OPENMG - OUTPUT BUFR FILE IS OPEN FOR '//
     . 'INPUT, IT MUST BE OPEN FOR OUTPUT')
      END
