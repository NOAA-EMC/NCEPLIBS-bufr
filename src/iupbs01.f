C> @file
C> @brief Read a data value from Section 0 or Section 1 of a BUFR
C> message.
C>
C> @author J. Ator @date 2005-11-29

C> Read a data value from Section 0 or Section 1 of a BUFR message.
C>
C> This function returns a specified value from within Section 0 or
C> Section 1 of a BUFR message.
C>
C> It will work on any BUFR message encoded using BUFR
C> edition 2, 3, or 4.  It is similar to function iupvs01(), except
C> that it operates on a BUFR message passed in via a memory array,
C> whereas iupvs01() operates on the BUFR message that was read into
C> internal arrays via the most recent call to any of the other
C> [message-reading subroutines](@ref hierarchy) for a specified
C> Fortran logical unit.
C>
C> @remarks
C> - The start of the BUFR message (i.e. the string 'BUFR') must be
C>   aligned on the first 4 bytes of MBAY.
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
C> @param[in] MBAY  - integer(*): BUFR message.
C> @param[in] S01MNEM - character*(*): Value to be read from
C> Section 0 or Section 1 of MBAY.
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
C> @returns iupbs01 - integer: Value corresponding to S01MNEM:
C> - -1 = S01MNEM was invalid for the edition of BUFR
C> message in MBAY, or some other error occurred
C>
C> @author J. Ator @date 2005-11-29

        RECURSIVE FUNCTION IUPBS01(MBAY,S01MNEM) RESULT(IRET)

        USE MODV_IM8B

        DIMENSION       MBAY(*)

        CHARACTER*(*)   S01MNEM

        LOGICAL         OK4CENT

C-----------------------------------------------------------------------
C       This statement function checks whether its input value contains
C       a valid century value.

        OK4CENT(IVAL) = ((IVAL.GE.19).AND.(IVAL.LE.21))
C-----------------------------------------------------------------------

C       Check for I8 integers.

        IF(IM8B) THEN
            IM8B=.FALSE.

            IRET = IUPBS01(MBAY,S01MNEM)

            IM8B=.TRUE.
            RETURN
        ENDIF

C       Call subroutine WRDLEN to initialize some important information
C       about the local machine, just in case subroutine OPENBF hasn't
C       been called yet.

        CALL WRDLEN

C       Handle some simple requests that do not depend on the BUFR
C       edition number.

        IF(S01MNEM.EQ.'LENM') THEN
            IRET = IUPB(MBAY,5,24)
            RETURN
        ENDIF

        LEN0 = 8
        IF(S01MNEM.EQ.'LEN0') THEN
            IRET = LEN0
            RETURN
        ENDIF

C       Get the BUFR edition number.

        IBEN = IUPB(MBAY,8,8)
        IF(S01MNEM.EQ.'BEN') THEN
            IRET = IBEN
            RETURN
        ENDIF

C       Use the BUFR edition number to handle any other requests.

        CALL GETS1LOC(S01MNEM,IBEN,ISBYT,IWID,IRETGS)
        IF(IRETGS.EQ.0) THEN
            IRET = IUPB(MBAY,LEN0+ISBYT,IWID)
            IF(S01MNEM.EQ.'CENT') THEN

C               Test whether the returned value was a valid
C               century value.

                IF(.NOT.OK4CENT(IRET)) IRET = -1
            ENDIF
        ELSE IF( (S01MNEM.EQ.'YEAR') .AND. (IBEN.LT.4) ) THEN

C           Calculate the 4-digit year.

            IYOC = IUPB(MBAY,21,8)
            ICEN = IUPB(MBAY,26,8)

C           Does ICEN contain a valid century value?

            IF(OK4CENT(ICEN)) THEN

C               YES, so use it to calculate the 4-digit year. Note that,
C               by international convention, the year 2000 was the 100th
C               year of the 20th century, and the year 2001 was the 1st
C               year of the 21st century

                IRET = (ICEN-1)*100 + IYOC
            ELSE

C               NO, so use a windowing technique to determine the
C               4-digit year from the year of the century.

                IRET = I4DY(MOD(IYOC,100)*1000000)/10**6
            ENDIF
        ELSE
            IRET = -1
        ENDIF

        RETURN
        END
