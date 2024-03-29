C> @file
C> @brief Read the next message from a BUFR file that was previously
C> opened for reading.
C>
C> @authors J. Woollen, J. Ator @date 1994-01-06

C> Reads the next BUFR message from logical unit
C> ABS(LUNXX) into internal arrays.
C>
C> Logical unit ABS(LUNXX) should have already been opened for
C> input operations via a previous call to subroutine openbf().
C>
C> Whenever this subroutine returns with IRET = 0, this indicates
C> that a new BUFR message of type SUBSET and date-time JDATE was
C> successfully read into internal arrays within the NCEPLIBS-bufr
C> software, and from where it can then be easily manipulated or further
C> parsed via a call to one of the
C> [subset-reading subroutines](@ref hierarchy). Otherwise,
C> if the subroutine returns with IRET = -1, then this indicates that
C> there are no more BUFR messages (i.e. end-of-file) within the file
C> connected to logical unit ABS(LUNXX).
C>
C> @remarks
C> - Any DX BUFR table messages encountered within ABS(LUNXX) will be
C> automatically processed and stored internally, so a successful return
C> from this subroutine will always result in a BUFR message containing
C> actual data values within the internal arrays.
C> - In prior versions of the NCEPLIBS-bufr software, an input value of
C> LUNXX < 0 was an indicator to the subroutine to treat any read error
C> from ABS(LUNXX) the same as an end-of-file condition. This option is
C> no longer supported, but the capability to call this subroutine with
C> LUNXX < 0 is itself still supported for backwards-compatibility with
C> certain legacy application programs.
C>
C> @param[in] LUNXX   -- integer: Absolute value is Fortran logical unit
C>                       number for BUFR file
C> @param[out] SUBSET  -- character*8: Table A mnemonic for type of BUFR
C>                        message that was read
C>                        (see [DX BUFR Tables](@ref dfbftab)
C>                        for further information about Table A mnemonics)
C> @param[out] JDATE   -- integer: Date-time stored within Section 1 of
C>                        BUFR message that was read, in format of either
C>                        YYMMDDHH or YYYYMMDDHH, depending on the most
C>                        recent call to subroutine datelen()
C> @param[out] IRET    -- integer: return code
C> - 0 = new BUFR message was successfully read into internal arrays.
C> - -1 = there are no more BUFR messages in the file connected to logical unit
C> ABS(LUNXX).
C>
C> @authors J. Woollen, J. Ator @date 1994-01-06
      RECURSIVE SUBROUTINE READMG(LUNXX,SUBSET,JDATE,IRET)

      use bufrlib

      use modv_vars, only: im8b

      use moda_msgcwd
      use moda_sc3bfr
      use moda_bitbuf

      COMMON /QUIET / IPRT

      CHARACTER*128 ERRSTR
      CHARACTER*8 SUBSET

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK FOR I8 INTEGERS
C  ---------------------

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84(LUNXX,MY_LUNXX,1)
         CALL READMG(MY_LUNXX,SUBSET,JDATE,IRET)
         CALL X48(JDATE,JDATE,1)
         CALL X48(IRET,IRET,1)

         IM8B=.TRUE.
         RETURN
      ENDIF

      IRET = 0
      LUNIT = ABS(LUNXX)

C  CHECK THE FILE STATUS
C  ---------------------

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IL.GT.0) GOTO 901
      CALL WTSTAT(LUNIT,LUN,IL,1)

C  READ A MESSAGE INTO THE INTERNAL MESSAGE BUFFER
C  -----------------------------------------------

1     CALL RDMSGW(LUNIT,MBAY(1,LUN),IER)
      IF(IER.EQ.-1) GOTO 200

C  PARSE THE MESSAGE SECTION CONTENTS
C  ----------------------------------

      IF(ISC3(LUN).NE.0) CALL READS3(LUN)
      CALL CKTABA(LUN,SUBSET,JDATE,IRET)

C  LOOK FOR A DICTIONARY MESSAGE
C  -----------------------------

      IF(IDXMSG(MBAY(1,LUN)).NE.1) RETURN

C     This is an internal dictionary message that was
C     generated by the NCEPLIBS-bufr software.

      IF(ISC3(LUN).NE.0) RETURN

C     Section 3 decoding isn't being used, so backspace the
C     file pointer and then use subroutine RDBFDX to read in
C     all such dictionary messages (they should be stored
C     consecutively!) and reset the internal tables.

      CALL BACKBUFR_C(LUN)
      CALL RDBFDX(LUNIT,LUN)

      IF(IPRT.GE.1) THEN
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
      ERRSTR = 'BUFRLIB: READMG - INTERNAL DICTIONARY MESSAGE READ;'//
     .' ACCOUNT FOR IT THEN READ IN NEXT MESSAGE WITHOUT RETURNING'
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
      CALL ERRWRT(' ')
      ENDIF

C     Now go read another message.

      GOTO 1

C  EOF ON ATTEMPTED READ
C  ---------------------

200   CALL WTSTAT(LUNIT,LUN,IL,0)
      INODE(LUN) = 0
      IDATE(LUN) = 0
      SUBSET = ' '
      JDATE = 0
      IRET = -1
      RETURN

C  EXITS
C  -----

900   CALL BORT('BUFRLIB: READMG - INPUT BUFR FILE IS CLOSED, IT MUST'//
     . ' BE OPEN FOR INPUT')
901   CALL BORT('BUFRLIB: READMG - INPUT BUFR FILE IS OPEN FOR OUTPUT'//
     . ', IT MUST BE OPEN FOR INPUT')
      END
