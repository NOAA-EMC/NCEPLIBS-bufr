
	PARAMETER	( MXR8PM = 6 )
	PARAMETER	( MXR8LV = 50 )
	
	CHARACTER	cmgtag*8,
     +                  cmeang1*40, cmeang2*40, cmeang3*40, cmeang4*40 

C*----------------------------------------------------------------------

	print *, '----------------------------------------------------'
	print *, 'testing BUFRLIB: reading IN_5'
	print *, '  using OPENBF IO = ''IN'' and LUNIN = LUNDX'
	print *, '  using PREPBUFR and code/flag table meaning strings'
	print *, '----------------------------------------------------'

	OPEN  ( UNIT = 11, FILE = 'testfiles/IN_5', FORM ='UNFORMATTED')

	CALL OPENBF ( 11, 'IN', 11 )

	print *, '        OPENBF -> OK'

        CALL MTINFO ( '../install/tables', 90, 91 )
        print *, '        MTINFO -> OK'

        CALL CODFLG ( 'Y' )

	print *, '        CODFLG -> OK'

	IF ( IREADNS ( 11, cmgtag, imgdt ) .eq. 0 ) THEN

	    print *, '       IREADNS -> OK'

C*	    Retrieve and check some code/flag meaning strings.

            CALL GETCFMNG
     +          ( 11, 'PRC', 106, ' ', -1, cmeang1, lcmg1, ier1 )
            CALL GETCFMNG
     +          ( 11, 'PRC', 106, 'PPC', 5, cmeang2, lcmg2, ier2 )
            CALL GETCFMNG
     +          ( 11, 'GSES', 10, ' ', -1, cmeang3, lcmg3, ier3 )
            CALL GETCFMNG
     +          ( 11, 'GSES', 10, 'GCLONG', 173, cmeang4, lcmg4, ier4 )

            IF ( ( ier1 .eq. 1 ) .and. ( lcmg1 .eq. 8 ) .and.
     +                  ( cmeang1(1:lcmg1) .eq. 'PPC     ' ) .and.
     +           ( ier2 .eq. 0 ) .and. ( lcmg2 .eq. 34 ) .and.
     +                  ( cmeang2(1:lcmg2) .eq.
     +                    'Surface pressure observation error' ) .and.
     +           ( ier3 .eq. 3 ) .and. ( lcmg3 .eq. 24  ) .and.
     +                  ( cmeang3(1:lcmg3) .eq.
     +                    'GCLONG  OGCE    ORIGC   ' ) .and.
     +           ( ier4 .eq. 0 ) .and. ( lcmg4 .eq. 20 ) .and.
     +                  ( cmeang4(1:lcmg4) .eq.
     +                    'Stennis Space Center' ) ) THEN
	        print *, '      GETCFMNG -> OK'
            ELSE
	        print *, '      GETCFMNG -> FAILED!!'
            ENDIF
	ENDIF

	STOP
	END
