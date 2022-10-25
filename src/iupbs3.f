C> @file
C> @brief Read a data value from Section 3 of a BUFR message.

C> This function returns a specified value from within Section 3
C> of a BUFR message.
C>
C> @author J. Ator
C> @date 2009-03-23
C>
C> @param[in]  MBAY   -- integer(*): BUFR message
C> @param[in]  S3MNEM  -- character*(*): Value to be read from
C>                        Section 3 of MBAY
C>                         - 'NSUB'  = Number of data subsets
C>                         - 'IOBS'  = Flag indicating whether the
C>                                     message contains observed data:
C>                                     - 0 = No
C>                                     - 1 = Yes
C>                         - 'ICMP'  = Flag indicating whether the
C>                                     message contains compressed data:
C>                                     - 0 = No
C>                                     - 1 = Yes
C> @returns iupbs3 -- integer: Value corresponding to S3MNEM
C>                      - -1 = S3MNEM was invalid 
C>
C> @remarks
C> - The start of the BUFR message (i.e. the string 'BUFR') must be
C>   aligned on the first 4 bytes of MBAY.
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 2009-03-23 | J. Ator | Original author |

	FUNCTION IUPBS3(MBAY,S3MNEM)

	DIMENSION	MBAY(*)

	CHARACTER*(*)	S3MNEM

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C	Call subroutine WRDLEN to initialize some important information
C	about the local machine, just in case subroutine OPENBF hasn't
C	been called yet.

	CALL WRDLEN

C	Skip to the beginning of Section 3.

	CALL GETLENS(MBAY,3,LEN0,LEN1,LEN2,LEN3,L4,L5)
	IPT = LEN0 + LEN1 + LEN2

C	Unpack the requested value.

	IF(S3MNEM.EQ.'NSUB') THEN
	    IUPBS3 = IUPB(MBAY,IPT+5,16)
	ELSE IF( (S3MNEM.EQ.'IOBS') .OR. (S3MNEM.EQ.'ICMP') ) THEN
	    IVAL = IUPB(MBAY,IPT+7,8)
	    IF(S3MNEM.EQ.'IOBS') THEN
		IMASK = 128
	    ELSE
		IMASK = 64
	    ENDIF
	    IUPBS3 = MIN(1,IAND(IVAL,IMASK))
	ELSE
	    IUPBS3 = -1
	ENDIF

	RETURN
	END
