C> @file
C> @author ATOR @date 2007-01-19
	
C> THIS SUBROUTINE PARSES AN ENTRY THAT WAS PREVIOUSLY READ
C>   FROM AN ASCII MASTER TABLE B FILE AND THEN STORES THE OUTPUT INTO
C>   THE MERGED ARRAYS.
C>
C> PROGRAM HISTORY LOG:
C> 2007-01-19  J. ATOR    -- ORIGINAL AUTHOR
C> 2021-01-08  J. ATOR    -- MODIFIED MSTABS ARRAY DECLARATIONS
C>                           FOR GNUv10 PORTABILITY
C> - 2021-05-17  J. Ator    -- Allow up to 24 characters in cmunit
C> - 2021-09-30  J. Ator    -- Replace jstchr with Fortran intrinsic
C>                             adjustl; replace rjust with Fortran
C>                             intrinsic adjustr
C>
C> USAGE:    CALL SNTBBE ( IFXYN, LINE, MXMTBB,
C>                         NMTBB, IMFXYN, CMSCL, CMSREF, CMBW,
C>                         CMUNIT, CMMNEM, CMDSC, CMELEM )
C>   INPUT ARGUMENT LIST:
C>     IFXYN    - INTEGER: BIT-WISE REPRESENTATION OF FXY NUMBER FOR
C>                TABLE ENTRY; THIS FXY NUMBER IS THE ELEMENT DESCRIPTOR
C>     LINE     - CHARACTER*(*): TABLE ENTRY
C>     MXMTBB   - INTEGER: MAXIMUM NUMBER OF ENTRIES TO BE STORED IN
C>                MERGED MASTER TABLE B ARRAYS; THIS SHOULD BE THE SAME
C>                NUMBER AS WAS USED TO DIMENSION THE OUTPUT ARRAYS IN
C>                THE CALLING PROGRAM, AND IT IS USED BY THIS SUBROUTINE
C>                TO ENSURE THAT IT DOESN'T OVERFLOW THESE ARRAYS
C>
C>   OUTPUT ARGUMENT LIST:
C>     NMTBB    - INTEGER: NUMBER OF ENTRIES IN MERGED MASTER TABLE B
C>                ARRAYS
C>     IMFXYN(*)- INTEGER: MERGED ARRAY CONTAINING BIT-WISE
C>                REPRESENTATIONS OF FXY NUMBERS (I.E. ELEMENT
C>                DESCRIPTORS)
C>     CMSCL(*) - CHARACTER*4: MERGED ARRAY CONTAINING SCALE FACTORS
C>     CMSREF(*)- CHARACTER*12: MERGED ARRAY CONTAINING REFERENCE VALUES
C>     CMBW(*)  - CHARACTER*4: MERGED ARRAY CONTAINING BIT WIDTHS
C>     CMUNIT(*)- CHARACTER*24: MERGED ARRAY CONTAINING UNITS
C>     CMMNEM(*)- CHARACTER*8: MERGED ARRAY CONTAINING MNEMONICS
C>     CMDSC(*) - CHARACTER*4: MERGED ARRAY CONTAINING DESCRIPTOR CODES 
C>     CMELEM(*)- CHARACTER*120: MERGED ARRAY CONTAINING ELEMENT NAMES 
C>
C> REMARKS:
C>    THIS ROUTINE CALLS:        BORT     BORT2    NEMOCK   PARSTR
C>    THIS ROUTINE IS CALLED BY: RDMTBB
C>                               Normally not called by any application
C>                               programs.
C>
	SUBROUTINE SNTBBE ( IFXYN, LINE, MXMTBB,
     .			    NMTBB, IMFXYN, CMSCL, CMSREF, CMBW,
     .			    CMUNIT, CMMNEM, CMDSC, CMELEM )



	CHARACTER*(*)	LINE
	CHARACTER*200	TAGS(10), WKTAG
	CHARACTER*128	BORT_STR1, BORT_STR2
	CHARACTER*4	CMDSC(*)
	CHARACTER	CMELEM(120,*)
	CHARACTER	CMUNIT(24,*)
	CHARACTER	CMSREF(12,*)
	CHARACTER	CMMNEM(8,*)
	CHARACTER	CMSCL(4,*), CMBW(4,*)

	INTEGER		IMFXYN(*)

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

	IF ( NMTBB .GE. MXMTBB ) GOTO 900
	NMTBB = NMTBB + 1

C	Store the FXY number.  This is the element descriptor.

	IMFXYN ( NMTBB ) = IFXYN

C	Parse the table entry.

	CALL PARSTR ( LINE, TAGS, 10, NTAG, '|', .FALSE. )
	IF ( NTAG .LT. 4 ) THEN
	    BORT_STR2 = '                  HAS TOO FEW FIELDS'
	    GOTO 901
	ENDIF

C	Scale factor.

  	TAGS(2) = ADJUSTL( TAGS(2) )
	IF ( TAGS(2) .EQ. ' ' ) THEN
	    BORT_STR2 = '                  HAS MISSING SCALE FACTOR'
	    GOTO 901
	ENDIF
	TAGS(2)(1:4) = ADJUSTR( TAGS(2)(1:4) )
        DO II = 1, 4
	    CMSCL ( II, NMTBB ) = TAGS(2)(II:II)
        ENDDO

C	Reference value.

	TAGS(3) = ADJUSTL( TAGS(3) )
	IF ( TAGS(3) .EQ. ' ' ) THEN
	    BORT_STR2 = '                  HAS MISSING REFERENCE VALUE'
	    GOTO 901
	ENDIF
	TAGS(3)(1:12) = ADJUSTR( TAGS(3)(1:12) )
        DO II = 1, 12
	    CMSREF ( II, NMTBB ) = TAGS(3)(II:II)
        ENDDO

C	Bit width.

	TAGS(4) = ADJUSTL( TAGS(4) )
	IF ( TAGS(4) .EQ. ' ' ) THEN
	    BORT_STR2 = '                  HAS MISSING BIT WIDTH'
	    GOTO 901
	ENDIF
	TAGS(4)(1:4) = ADJUSTR( TAGS(4)(1:4) )
        DO II = 1, 4
	    CMBW ( II, NMTBB ) = TAGS(4)(II:II)
        END DO

C	Units.  Note that this field is allowed to be blank.

	IF ( NTAG .GT. 4 ) THEN
	    TAGS(5) = ADJUSTL( TAGS(5) )
            DO II = 1, 24
	        CMUNIT ( II, NMTBB ) = TAGS(5)(II:II)
            ENDDO
	ELSE
            DO II = 1, 24
	        CMUNIT ( II, NMTBB ) = ' '
            ENDDO
	ENDIF

C	Comment (additional) fields.  Any of these fields may be blank.

	CMDSC ( NMTBB ) = ' '
        DO II = 1, 8
	    CMMNEM ( II, NMTBB ) = ' '
        ENDDO
        DO II = 1, 120 
	    CMELEM ( II, NMTBB ) = ' '
        ENDDO
	IF ( NTAG .GT. 5 ) THEN
	    WKTAG = TAGS(6)
	    CALL PARSTR ( WKTAG, TAGS, 10, NTAG, ';', .FALSE. )
	    IF ( NTAG .GT. 0 ) THEN
C		The first additional field contains the mnemonic.
		TAGS(1) = ADJUSTL( TAGS(1) )
C		If there is a mnemonic, then make sure it's legal.
		IF ( ( TAGS(1) .NE. ' ' ) .AND.
     .		    ( NEMOCK ( TAGS(1) ) .NE. 0 ) ) THEN
		    BORT_STR2 = '                  HAS ILLEGAL MNEMONIC'
		    GOTO 901
		ENDIF
                DO II = 1, 8
		    CMMNEM ( II, NMTBB ) = TAGS(1)(II:II)
                ENDDO
	    ENDIF
	    IF ( NTAG .GT. 1 ) THEN
C		The second additional field contains descriptor codes.
		TAGS(2) = ADJUSTL( TAGS(2) )
		CMDSC ( NMTBB ) = TAGS(2)(1:4)
	    ENDIF
	    IF ( NTAG .GT. 2 ) THEN
C		The third additional field contains the element name.
		TAGS(3) = ADJUSTL( TAGS(3) )
                DO II = 1, 120 
		    CMELEM ( II, NMTBB ) = TAGS(3)(II:II)
                ENDDO
	    ENDIF
	ENDIF

	RETURN
 900	CALL BORT('BUFRLIB: SNTBBE - OVERFLOW OF MERGED ARRAYS')
 901	BORT_STR1 = 'BUFRLIB: SNTBBE - CARD BEGINNING WITH: ' //
     .     LINE(1:20)
	CALL BORT2(BORT_STR1,BORT_STR2)
	END
