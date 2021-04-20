C> @file
C> @brief Read the date-time from Section 1 of a BUFR message.

C> This function returns the date-time from within Section 1 of a
C> BUFR message.
C>
C> <p>The function will work on any BUFR message encoded using BUFR
C> edition 2, 3, or 4.
C>
C> @author J. Ator
C> @date 2005-11-29
C>
C> @param[in]  MBAY    - integer(*): BUFR message
C> @param[out] IYR    - integer: Year stored within Section 1 of MBAY,
C>                      in format of either YY or YYYY, depending on
C>                      the most recent call to subroutine datelen()
C> @param[out] IMO    - integer: Month stored within Section 1 of MBAY
C> @param[out] IDY    - integer: Day stored within Section 1 of MBAY
C> @param[out] IHR    - integer: Hour stored within Section 1 of MBAY
C> @returns igetdate    - integer: Date-time stored within Section 1
C>                        of MBAY, in format of either YYMMDDHH or
C>                        YYYYMMDDHH, depending on the most
C>                        recent call to subroutine datelen()
C>
C> @remarks
C> - The start of the BUFR message (i.e. the string 'BUFR') must be
C>   aligned on the first 4 bytes of MBAY.
C>
	FUNCTION IGETDATE(MBAY,IYR,IMO,IDY,IHR)

	COMMON /DATELN/ LENDAT

	DIMENSION MBAY(*)

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

	IYR = IUPBS01(MBAY,'YEAR')
	IMO = IUPBS01(MBAY,'MNTH')
	IDY = IUPBS01(MBAY,'DAYS')
	IHR = IUPBS01(MBAY,'HOUR')
	IF(LENDAT.NE.10) THEN
	    IYR = MOD(IYR,100)
	ENDIF
	IGETDATE = (IYR*1000000) + (IMO*10000) + (IDY*100) + IHR

	RETURN
	END
