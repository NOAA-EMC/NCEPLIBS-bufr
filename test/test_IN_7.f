        MODULE Share_errstr

C>          This module is needed in order to share information
C>          between the test program and subroutine ERRWRT, because
C>          the latter is not called by the former but rather is
C>          called directly from within the BUFRLIB software.

            CHARACTER*1500       errstr

            INTEGER              errstr_len

        END MODULE

C>-------------------------------------------------------------------

        SUBROUTINE ERRWRT(str)

C>      This subroutine supersedes the subroutine of the same name
C>      from the BUFRLIB software, so that we can easily test the
C>      generation of error messages from within the library.

        USE Share_errstr

        CHARACTER*(*)   str

        INTEGER         str_len 

        str_len = LEN(str)
        errstr ( errstr_len + 1 : errstr_len + str_len + 1 ) = str
        errstr_len = errstr_len + str_len

        RETURN
        END

C>-------------------------------------------------------------------

	use bufr_procedures
        use Share_errstr

	PARAMETER	( MXR8PM = 15 )
	PARAMETER	( MXR8LV = 5 )
	
	REAL*8		r8arr ( MXR8PM, MXR8LV ), r8val

	CHARACTER	cmgtag*8

	print *, '----------------------------------------------------'
	print *, 'testing BUFRLIB: reading IN_7'
	print *, '  using 2-03-YYY changed reference values'
	print *, '  using inline ERRWRT to check error messages'
	print *, '  using UFBPOS, UFBTAB, and VALX'
	print *, '----------------------------------------------------'

        iret1 = ISETPRM ( 'MXNRV', 5 )
        errstr_len = 1
        iret2 = ISETPRM ( 'DUMMY', 20 ) 
        IF ( ( iret1 .eq. 0 ) .and. ( iret2 .eq. -1 ) .and.
     +      ( INDEX( errstr(1:errstr_len),
     +               'ISETPRM - UNKNOWN INPUT PARAMETER DUMMY' )
     +        .ne. 0 ) ) THEN 
	    print *, '       ISETPRM -> OK'
	ELSE
	    print *, '       ISETPRM -> FAILED!!'
        END IF

	OPEN  ( UNIT = 11, FILE = 'testfiles/IN_7', FORM ='UNFORMATTED')
        OPEN  ( UNIT = 12, FILE = 'testfiles/IN_7_bufrtab' )

        CALL OPENBF  ( 11, 'IN', 12 )
        CALL OPENBF  ( 11, 'QUIET', 1 )

        iret1 = IGETPRM ( 'MXNRV' )
        errstr_len = 1
        iret2 = IGETPRM ( 'DUMMY' )
        IF ( ( iret1 .eq. 5 ) .and. ( iret2 .eq. -1 ) .and.
     +      ( INDEX( errstr(1:errstr_len),
     +               'IGETPRM - UNKNOWN INPUT PARAMETER DUMMY' )
     +        .ne. 0 ) ) THEN 
	    print *, '       IGETPRM -> OK'
	ELSE
	    print *, '       IGETPRM -> FAILED!!'
        END IF

C>      Read some data values from the 1st messaage, which uses the
C>      2-03-YYY operator to change one of the reference values.

        IF ( IREADNS ( 11, cmgtag, imgdt ) .ne. 0 ) THEN
	    print *, '       IREADNS -> FAILED!!'
        ELSE
	    print *, '       IREADNS -> OK'
            CALL UFBREP ( 11, r8arr, MXR8PM, MXR8LV, nr8a, 'TIDER' )
            errstr_len = 1
            CALL UFBREP ( 11, r8val, 1, 1, nr8v, 'DUMMY' )
            idx1 = INDEX( errstr(1:errstr_len),
     +                    'UFBREP - NO SPECIFIED VALUES READ IN' )
            errstr_len = 1
            CALL UFBREP ( 11, r8val, 0, 1, nr8v2, 'TIDER' )
            idx2 = INDEX( errstr(1:errstr_len),
     +                    'UFBREP - 3rd ARG. (INPUT) IS .LE. 0' )
            IF ( ( nr8a .eq. 2 ) .and.
     +          ( nr8v .eq. 0 ) .and. ( nr8v2 .eq. 0 ) .and.
     +          ( idx1 .gt. 0 ) .and. ( idx2 .gt. 0 ) .and.
     +          ( IDNINT ( r8arr(1,1) ) .eq. -10000 ) .and.
     +          ( IDNINT ( r8arr(1,2) ) .eq. 16 ) ) THEN
	        print *, '        UFBREP -> OK'
            ELSE
	        print *, '        UFBREP -> FAILED!!'
            END IF
        END IF
            
C>      Jump ahead to the 5th subset of the 23rd message and read
C>      some data values.

        CALL UFBPOS ( 11, 23, 5, cmgtag, jdate )
        CALL UFBINT ( 11, r8arr, MXR8PM, MXR8LV, nr8a,
     +                'CLATH CLONH TMDB SWRAD' )
        errstr_len = 1
        CALL UFBINT ( 11, r8val, 1, 1, nr8v, 'DUMMY' )
        idx1 = INDEX( errstr(1:errstr_len),
     +                'UFBINT - NO SPECIFIED VALUES READ IN' )
        errstr_len = 1
        CALL UFBINT ( 11, r8val, 1, 0, nr8v2, 'TMDB' )
        idx2 = INDEX( errstr(1:errstr_len),
     +                'UFBINT - 4th ARG. (INPUT) IS .LE. 0' )
        IF ( ( nr8a .eq. 1 ) .and.
     +      ( nr8v .eq. 0 ) .and. ( nr8v2 .eq. 0 ) .and.
     +      ( idx1 .gt. 0 ) .and. ( idx2 .gt. 0 ) .and.
     +      ( IDNINT ( r8arr(1,1)*100000 ) .eq. 2001191 ) .and.
     +      ( IDNINT ( r8arr(2,1)*100000 ) .eq. -3785017 ) .and.
     +      ( IDNINT ( r8arr(3,1)*100 ) .eq. 30035 ) .and.
     +      ( IDNINT ( r8arr(4,1) ) .eq. 2187000 ) ) THEN
	    print *, '        UFBPOS -> OK'
	    print *, '        UFBINT -> OK'
        ELSE
	    print *, '        UFBPOS -> FAILED!!'
	    print *, '        UFBINT -> FAILED!!'
        END IF
            
C>      Jump ahead to the 2nd subset of the 30th message and read
C>      some data values.

        CALL UFBPOS ( 11, 30, 2, cmgtag, jdate )
        CALL UFBSTP ( 11, r8arr, MXR8PM, MXR8LV, nr8a,
     +                'CLAT CLON HSMSL' )
        errstr_len = 1
        CALL UFBSTP ( 11, r8val, 1, 1, nr8v, 'DUMMY' )
        idx1 = INDEX( errstr(1:errstr_len),
     +                'UFBSTP - NO SPECIFIED VALUES READ IN' )
        errstr_len = 1
        CALL UFBSTP ( 11, r8val, 1, 0, nr8v2, 'CLON' )
        idx2 = INDEX( errstr(1:errstr_len),
     +                'UFBSTP - 4th ARG. (INPUT) IS .LE. 0' )
        IF ( ( nr8a .eq. 1 ) .and.
     +      ( nr8v .eq. 0 ) .and. ( nr8v2 .eq. 0 ) .and.
     +      ( idx1 .gt. 0 ) .and. ( idx2 .gt. 0 ) .and.
     +      ( IDNINT ( r8arr(1,1)*100 ) .eq. 3163 ) .and.
     +      ( IDNINT ( r8arr(2,1)*100 ) .eq. -11017 ) .and.
     +      ( IDNINT ( r8arr(3,1) ) .eq. 1205 ) ) THEN
	    print *, '        UFBPOS -> OK'
	    print *, '        UFBSTP -> OK'
        ELSE
	    print *, '        UFBPOS -> FAILED!!'
	    print *, '        UFBSTP -> FAILED!!'
        END IF
            
C>      Jump backwards to the 88th subset of the 29th message and read
C>      some data values.

        CALL UFBPOS ( 11, 29, 88, cmgtag, jdate )
        CALL UFBSEQ ( 11, r8arr, MXR8PM, MXR8LV, nr8a,
     +                'NC008023' )
        errstr_len = 1
        CALL UFBSEQ ( 11, r8val, 1, 1, nr8v, 'DUMMY' )
        idx1 = INDEX( errstr(1:errstr_len),
     +                'UFBSEQ - NO SPECIFIED VALUES READ IN' )
        errstr_len = 1
        CALL UFBSEQ ( 11, r8val, 0, 1, nr8v2, 'CLON' )
        idx2 = INDEX( errstr(1:errstr_len),
     +                'UFBSEQ - 3rd ARG. (INPUT) IS .LE. 0' )
        IF ( ( nr8a .eq. 1 ) .and.
     +      ( nr8v .eq. 0 ) .and. ( nr8v2 .eq. 0 ) .and.
     +      ( idx1 .gt. 0 ) .and. ( idx2 .gt. 0 ) .and.
     +      ( IDNINT ( r8arr(6,1)*100000 ) .eq. 2967000 ) .and.
     +      ( IDNINT ( r8arr(7,1)*100000 ) .eq. -9512833 ) .and.
     +      ( IDNINT ( r8arr(5,1) ) .eq. 482011039 ) ) THEN
	    print *, '        UFBPOS -> OK'
	    print *, '        UFBSEQ -> OK'
        ELSE
	    print *, '        UFBPOS -> FAILED!!'
	    print *, '        UFBSEQ -> FAILED!!'
        END IF

C>      Rewind the file and get a total count of the subsets.

        CALL UFBTAB ( -11, r8val, 1, 1, nsub, ' ' )
        IF ( ( nsub .eq. 402 ) .and. ( IBFMS ( r8val ) .eq. 1 ) ) THEN
	    print *, '        UFBTAB -> OK'
        ELSE
	    print *, '        UFBTAB -> FAILED!!'
        END IF

C>      Test the error handling inside of VALX.

        errstr_len = 1
        r8val = VALX ( '75.DUMMY' )
        IF ( ( INDEX( errstr(1:errstr_len),
     +               'VALX - ERROR READING STRING' ) .ne. 0 ) ) THEN 
	    print *, '          VALX -> OK'
	ELSE
	    print *, '          VALX -> FAILED!!'
        END IF

	STOP
	END
