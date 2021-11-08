C> @file
C> @brief Write a data value into Section 1 of a BUFR message.

C> This subroutines writes a specified value into a specified location
C> within Section 1 of a BUFR message, overwriting the value previously
C> stored in that location.
C>
C> <p>This subroutine will work on any BUFR message encoded using BUFR
C> edition 2, 3, or 4.  It is similar to subroutine pkvs01(), except
C> that it operates on a BUFR message passed in via a memory array,
C> whereas pkvs01() operates on BUFR messages stored internally within
C> the software.
C>
C> @authors J. Ator
C> @authors D. Keyser
C> @date 2005-11-29
C>	
C> @param[in]     IVAL   -- integer: Value to be stored
C> @param[in,out] MBAY   -- integer(*): BUFR message
C> @param[in]   S1MNEM   -- character*(*): Location in Section 1 of
C>                          MBAY within which to store IVAL
C>                          - 'BMT'   = BUFR master table
C>                          - 'OGCE'  = Originating center
C>                          - 'GSES'  = Originating subcenter
C>                          - 'USN'   = Update sequence number
C>                          - 'MTYP'  = Data category
C>                          - 'MSBTI' = Data subcategory (international)
C>                          - 'MSBT'  = Data subcategory (local)
C>                          - 'MTV'   = Version number of master table
C>                          - 'MTVL'  = Version number of local tables
C>                          - 'YCEN'  = Year of century (1-100)
C>                          - 'CENT'  = Century (e.g., 20 for years 1901-2000,
C>                                      21 for years 2001-2100)
C>                          - 'YEAR'  = Year (4-digit)
C>                          - 'MNTH'  = Month
C>                          - 'DAYS'  = Day
C>                          - 'HOUR'  = Hour
C>                          - 'MINU'  = Minute
C>                          - 'SECO'  = Second
C>
C> @remarks
C> - The start of the BUFR message (i.e. the string 'BUFR') must be
C>   aligned on the first 4 bytes of MBAY.
C> - Values corresponding to S1MNEM = 'GSES' can only be stored within
C>   BUFR messages encoded using BUFR edition 3 or 4.
C> - Values corresponding to S1MNEM = 'YCEN' or 'CENT' can only be stored
C>   within BUFR messages encoded using BUFR edition 2 or 3.
C> - Values corresponding to S1MNEM = 'YEAR', 'SECO' or 'MSBTI' can only
C>   be stored within BUFR messages encoded using BUFR edition 4.
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 2005-11-29 | J. Ator    | Original author |
C> | 2006-04-14 | D. Keyser  | Added options for 'MTYP', 'MSBT', 'YEAR', 'MNTH', 'DAYS', 'HOUR', 'YCEN' and 'CENT' |
C>
	SUBROUTINE PKBS1(IVAL,MBAY,S1MNEM)

	DIMENSION	MBAY(*)

	CHARACTER*(*)	S1MNEM

	CHARACTER*128	BORT_STR

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C	Note that the following call to function IUPBS01 will ensure
C	that subroutine WRDLEN has been called.

	IBEN = IUPBS01(MBAY,'BEN')

C	Determine where to store the value.

	CALL GETS1LOC(S1MNEM,IBEN,ISBYT,IWID,IRET)
	IF ( (IRET.EQ.0) .AND.
     .	     ( (S1MNEM.EQ.'USN') .OR. (S1MNEM.EQ.'BMT')   .OR.
     .	       (S1MNEM.EQ.'OGCE') .OR. (S1MNEM.EQ.'GSES')  .OR.
     .	       (S1MNEM.EQ.'MTYP') .OR. (S1MNEM.EQ.'MSBTI') .OR.
     .	       (S1MNEM.EQ.'MSBT') .OR. (S1MNEM.EQ.'MTV')   .OR.
     .	       (S1MNEM.EQ.'MTVL') .OR. (S1MNEM.EQ.'YCEN')  .OR.
     .	       (S1MNEM.EQ.'CENT') .OR. (S1MNEM.EQ.'YEAR')  .OR.
     .	       (S1MNEM.EQ.'MNTH') .OR. (S1MNEM.EQ.'DAYS')  .OR.
     .	       (S1MNEM.EQ.'HOUR') .OR. (S1MNEM.EQ.'MINU')  .OR.
     .	       (S1MNEM.EQ.'SECO') ) ) THEN

C	    Store the value.

	    IBIT = (IUPBS01(MBAY,'LEN0')+ISBYT-1)*8
	    CALL PKB(IVAL,IWID,MBAY,IBIT)
	ELSE
	    GOTO 900
	ENDIF

	RETURN
900	WRITE(BORT_STR,'("BUFRLIB: PKBS1 - CANNOT OVERWRITE LOCATION '//
     .	    'CORRESPONDING TO MNEMONIC (",A,") WITHIN BUFR EDITION '//
     .	    '(",I1,")")') S1MNEM, IBEN
      	CALL BORT(BORT_STR)
	END
