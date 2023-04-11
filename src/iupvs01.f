C> @file
C> @brief Read a data value from Section 0 or Section 1 of a BUFR
C> message.
C>
C> @author J. Ator @date 2005-11-29

C> This function returns a specified value from within Section 0 or
C> Section 1 of a BUFR message.
C>
C> This function will work on any BUFR message encoded using BUFR
C> edition 2, 3, or 4.  It is similar to function iupbs01(), except
C> that iupbs01() operates on a BUFR message passed in via a memory
C> array, whereas this function operates on the BUFR message that was
C> read into internal arrays via the most recent call to any of the
C> other [message-reading subroutines](@ref hierarchy) for a specified
C> Fortran logical unit.
C>
C> @param[in] LUNIT - integer: Fortran logical unit number for BUFR file
C> @param[in] S01MNEM - character*(*): Value to be read from Section 0
C> or Section 1 of BUFR message in internal arrays for LUNIT:
C> - 'LENM'  = Length (in bytes) of BUFR message
C> - 'LEN0'  = Length (in bytes) of Section 0
C> - 'LEN1'  = Length (in bytes) of Section 1
C> - 'BEN'   = BUFR edition number
C> - 'BMT'   = BUFR master table
C> - 'OGCE'  = Originating center
C> - 'GSES'  = Originating subcenter
C> - 'USN'   = Update sequence number
C> - 'ISC2'  = Flag indicating absence/presence of (optional) Section 2
C>   in BUFR message:
C>   - 0 = Section 2 absent
C>   - 1 = Section 2 present
C> - 'MTYP'  = Data category
C> - 'MSBTI' = Data subcategory (international)
C> - 'MSBT'  = Data subcategory (local)
C> - 'MTV'   = Version number of master table
C> - 'MTVL'  = Version number of local tables
C> - 'YCEN'  = Year of century (1-100)
C> - 'CENT'  = Century (e.g., 20 for years 1901-2000, 21 for years 2001-2100)
C> - 'YEAR'  = Year (4-digit)
C> - 'MNTH'  = Month
C> - 'DAYS'  = Day
C> - 'HOUR'  = Hour
C> - 'MINU'  = Minute
C> - 'SECO'  = Second
C> @returns iupvs01 -- integer: Value corresponding to S01MNEM:
C> - -1 = S01MNEM was invalid for the edition of BUFR message in
C> internal arrays for LUNIT, or some other error occurred
C>
C> @remarks
C> - Values corresponding to S01MNEM = 'GSES' can only be read from
C>   BUFR messages encoded using BUFR edition 3 or 4.
C> - Values corresponding to S01MNEM = 'YCEN' or 'CENT' can only be
C>   read from BUFR messages encoded using BUFR edition 2 or 3.
C> - When reading from BUFR messages encoded using BUFR edition 2
C>   or 3, values corresponding to S01MNEM = 'YEAR' will be
C>   calculated internally using the values for 'YCEN' and 'CENT',
C>   or inferred using a windowing technique
C> - Values corresponding to S01MNEM = 'SECO' or 'MSBTI' can only
C>   be read from BUFR messages encoded using BUFR edition 4.
C>
C> @author J. Ator @date 2005-11-29
      RECURSIVE FUNCTION IUPVS01(LUNIT,S01MNEM) RESULT(IRET)

      USE MODV_IM8B

      USE MODA_BITBUF

      CHARACTER*(*)   S01MNEM

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK FOR I8 INTEGERS
C  ---------------------

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84(LUNIT,MY_LUNIT,1)
         IRET=IUPVS01(MY_LUNIT,S01MNEM)

         IM8B=.TRUE.
         RETURN
      ENDIF

      IRET = -1

C  CHECK THE FILE STATUS
C  ---------------------

      CALL STATUS(LUNIT,LUN,ILST,IMST)
      IF(ILST.EQ.0) GOTO 900
      IF(ILST.GT.0) GOTO 901
      IF(IMST.EQ.0) GOTO 902

C  UNPACK THE REQUESTED VALUE
C  --------------------------

      IRET = IUPBS01(MBAY(1,LUN),S01MNEM)

C  EXITS
C  -----

      RETURN
900   CALL BORT('BUFRLIB: IUPVS01 - INPUT BUFR FILE IS CLOSED, IT '//
     . 'MUST BE OPEN FOR INPUT')
901   CALL BORT('BUFRLIB: IUPVS01 - INPUT BUFR FILE IS OPEN FOR '//
     . 'OUTPUT, IT MUST BE OPEN FOR INPUT')
902   CALL BORT('BUFRLIB: IUPVS01 - A MESSAGE MUST BE OPEN IN INPUT '//
     . 'BUFR FILE, NONE ARE')
      END

      
