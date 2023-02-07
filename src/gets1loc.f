C> @file
C> @brief Get the location of a specified value within Section 1
C> of a BUFR message

C> This subroutine returns the location of a specified value within
C> Section 1 of a BUFR message.
C>
C> <p>The location and availability of any particular value within
C> Section 1 of a BUFR message can vary depending on the edition
C> number used to encode the message.  This subroutine will work
C> for BUFR edition 2, 3, or 4.
C>
C> @author J. Ator
C> @date 2005-11-29
C>
C> @param[in]  S1MNEM  -- character*(*): Value whose location within
C>                        Section 1 is to be determined:
C>                         - 'LEN1'  = Length (in bytes) of Section 1
C>                         - 'BMT'   = BUFR master table
C>                         - 'OGCE'  = Originating center
C>                         - 'GSES'  = Originating subcenter
C>                         - 'USN'   = Update sequence number
C>                         - 'ISC2'  = Flag indicating absence/presence of
C>                                     (optional) Section 2 in BUFR message:
C>                                    - 0 = Section 2 absent
C>                                    - 1 = Section 2 present
C>                         - 'MTYP'  = Data category
C>                         - 'MSBTI' = Data subcategory (international)
C>                         - 'MSBT'  = Data subcategory (local)
C>                         - 'MTV'   = Version number of master table
C>                         - 'MTVL'  = Version number of local tables
C>                         - 'YCEN'  = Year of century (1-100)
C>                         - 'CENT'  = Century (e.g., 20 for years 1901-2000,
C>                                     21 for years 2001-2100)
C>                         - 'YEAR'  = Year (4-digit)
C>                         - 'MNTH'  = Month
C>                         - 'DAYS'  = Day
C>                         - 'HOUR'  = Hour
C>                         - 'MINU'  = Minute
C>                         - 'SECO'  = Second
C> @param[in]  IBEN    -- integer: BUFR edition number
C> @param[out] ISBYT   -- integer: Number of starting byte within Section 1
C>                        which contains value corresponding to S1MNEM
C> @param[out] IWID    -- integer: Width (in bits) of value corresponding
C>                        to S1MNEM, counting from the first bit of the
C>                        byte pointed to by ISBYT
C> @param[out] IRET    -- integer: Return code:
C>                        - 0 = normal return
C>                        - -1 = S1MNEM is invalid for BUFR edition IBEN
C>
C> @remarks
C> - S1MNEM = 'GSES' is only valid for IBEN = 3 or 4.
C> - S1MNEM = 'YCEN' or 'CENT' is only valid for IBEN = 2 or 3.
C> - S1MNEM = 'YEAR', 'SECO', or 'MSBTI' is only valid for IBEN = 4.
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 2005-11-29 | J. Ator | Original author |
C> | 2006-04-14 | D. Keyser | Added options for 'YCEN' and 'CENT' |
C> | 2022-08-04 | J. Woollen | Added 8-byte wrapper |

        RECURSIVE SUBROUTINE GETS1LOC(S1MNEM,IBEN,ISBYT,IWID,IRET)

        USE MODV_IM8B

        CHARACTER*(*) S1MNEM

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C       Check for I8 integers.

        IF(IM8B) THEN
           IM8B=.FALSE.

           CALL X84(IBEN,MY_IBEN,1)
           CALL GETS1LOC(S1MNEM,MY_IBEN,ISBYT,IWID,IRET)
           CALL X48(ISBYT,ISBYT,1)
           CALL X48(IWID,IWID,1)
           CALL X48(IRET,IRET,1)

           IM8B=.TRUE.
           RETURN
        ENDIF

        IRET = 0
        IWID = 8

        IF(S1MNEM.EQ.'LEN1') THEN
            ISBYT = 1
            IWID = 24
        ELSE IF(S1MNEM.EQ.'BMT') THEN
            ISBYT = 4
        ELSE IF(S1MNEM.EQ.'OGCE') THEN
            IF(IBEN.EQ.3) THEN
                ISBYT = 6
            ELSE

C               Note that this location is actually the same for both
C               Edition 2 *and* Edition 4 of BUFR!

                ISBYT = 5
                IWID = 16
            ENDIF
        ELSE IF(S1MNEM.EQ.'GSES') THEN
            IF(IBEN.EQ.3) THEN
                ISBYT = 5
            ELSE IF(IBEN.EQ.4) THEN
                ISBYT = 7
                IWID = 16
            ELSE
                IRET = -1
            ENDIF
        ELSE IF(S1MNEM.EQ.'USN') THEN
            IF(IBEN.EQ.4) THEN
                ISBYT = 9
            ELSE
                ISBYT = 7
            ENDIF
        ELSE IF(S1MNEM.EQ.'ISC2') THEN
            IWID = 1
            IF(IBEN.EQ.4) THEN
                ISBYT = 10
            ELSE
                ISBYT = 8
            ENDIF
        ELSE IF(S1MNEM.EQ.'MTYP') THEN
            IF(IBEN.EQ.4) THEN
                ISBYT = 11
            ELSE
                ISBYT = 9
            ENDIF
        ELSE IF(S1MNEM.EQ.'MSBTI') THEN
            IF(IBEN.EQ.4) THEN
                ISBYT = 12
            ELSE
                IRET = -1
            ENDIF
        ELSE IF(S1MNEM.EQ.'MSBT') THEN
            IF(IBEN.EQ.4) THEN
                ISBYT = 13
            ELSE
                ISBYT = 10
            ENDIF
        ELSE IF(S1MNEM.EQ.'MTV') THEN
            IF(IBEN.EQ.4) THEN
                ISBYT = 14
            ELSE
                ISBYT = 11
            ENDIF
        ELSE IF(S1MNEM.EQ.'MTVL') THEN
            IF(IBEN.EQ.4) THEN
                ISBYT = 15
            ELSE
                ISBYT = 12
            ENDIF
        ELSE IF(S1MNEM.EQ.'YEAR') THEN
            IF(IBEN.EQ.4) THEN
                ISBYT = 16
                IWID = 16
            ELSE
                IRET = -1
            ENDIF
        ELSE IF(S1MNEM.EQ.'YCEN') THEN
            IF(IBEN.LT.4) THEN
                ISBYT = 13
            ELSE
                IRET = -1
            ENDIF
        ELSE IF(S1MNEM.EQ.'CENT') THEN
            IF(IBEN.LT.4) THEN
                ISBYT = 18
            ELSE
                IRET = -1
            ENDIF
        ELSE IF(S1MNEM.EQ.'MNTH') THEN
            IF(IBEN.EQ.4) THEN
                ISBYT = 18
            ELSE
                ISBYT = 14
            ENDIF
        ELSE IF(S1MNEM.EQ.'DAYS') THEN
            IF(IBEN.EQ.4) THEN
                ISBYT = 19
            ELSE
                ISBYT = 15
            ENDIF
        ELSE IF(S1MNEM.EQ.'HOUR') THEN
            IF(IBEN.EQ.4) THEN
                ISBYT = 20
            ELSE
                ISBYT = 16
            ENDIF
        ELSE IF(S1MNEM.EQ.'MINU') THEN
            IF(IBEN.EQ.4) THEN
                ISBYT = 21
            ELSE
                ISBYT = 17
            ENDIF
        ELSE IF(S1MNEM.EQ.'SECO') THEN
            IF(IBEN.EQ.4) THEN
                ISBYT = 22
            ELSE
                IRET = -1
            ENDIF
        ELSE
            IRET = -1
        ENDIF

        RETURN
        END
