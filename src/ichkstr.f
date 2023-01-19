C> @file
C> @brief Compare a specified number of characters
c> from an input character array against the same number of characters
c> from an input character string and determines whether the two are
c> equivalent. 	
C> @author Ator @date 2005-11-29
	
C> This function compares a specified number of characters
c> from an input character array against the same number of characters
c> from an input character string and determines whether the two are
c> equivalent. The character array is assumed to be in ASCII, whereas
c> the character string is assumed to be in the native character set
c> (i.e. ASCII or EBCDIC) of the local machine.
C>
C> @param[in] STR - character*(*): n-character string in ASCII or
C> EBCDIC, depending on the native machine.
C> @param[in] CHR - character*1: array of n characters in ASCII.
C> @param[in] N - integer: number of characters to be compared.
C>
C> @return
C> - 0 STR(1:N) and (CHR(I),I=1,N) are equivalent.
C> - 1 STR(1:N) and (CHR(I),I=1,N) are not equivalent.
C>
C> @author Ator @date 2005-11-29
	FUNCTION ICHKSTR(STR,CHR,N)



	CHARACTER*(*) STR

	CHARACTER*80  CSTR
	CHARACTER*1   CHR(N)

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C	Copy CHR into CSTR and, if necessary, convert the latter
C	to EBCDIC (i.e. if the local machine uses EBCDIC) so that
C	the subsequent comparison will always be valid.

	CALL CHRTRNA(CSTR,CHR,N)

C	Compare CSTR to STR.

	IF(CSTR(1:N).EQ.STR(1:N)) THEN
	    ICHKSTR = 0
	ELSE
	    ICHKSTR = 1
	ENDIF	

	RETURN
	END
