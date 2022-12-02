#ifdef INTSIZE_8
	INTEGER*4	NMSUB
#endif

	CHARACTER	cmgtag*8

C*----------------------------------------------------------------------

	print *, '----------------------------------------------------'
	print *, 'testing BUFRLIB: reading IN_6'
	print *, '  using UFBMEM, RDMEMM and UFBMNS'
	print *, '----------------------------------------------------'

#ifdef INTSIZE_8
	CALL SETIM8B ( .true. )
#endif

	OPEN  ( UNIT = 21, FILE = 'testfiles/IN_6_infile1',
     +          FORM = 'UNFORMATTED')
	OPEN  ( UNIT = 22, FILE = 'testfiles/IN_6_infile2',
     +          FORM = 'UNFORMATTED')

        CALL DATEBF ( 22, iyr, imon, iday, ihour, imgdt )
        IF ( ( imgdt .eq. 21031900 ) .and.
     +       ( iyr .eq. 21 ) .and. ( iday .eq. 19 ) ) THEN
            print *, '        DATEBF -> OK'
        ELSE
            print *, '        DATEBF -> FAILED!!'
        END IF
        REWIND ( 22 )
        
C*      Open the input files.

        CALL UFBMEM ( 21, 0, icnt1, iunt1 )
        CALL UFBMEM ( 22, 1, icnt2, iunt2 )

        IF ( ( icnt1 .eq. 926 ) .and. ( icnt2 .eq. 344 ) .and.
     +       ( iunt1 .eq. 21 ) .and. ( iunt2 .eq. 21 ) ) THEN
            print *, '        UFBMEM -> OK'
        ELSE
            print *, '        UFBMEM -> FAILED!!'
        END IF

C*      Read message #167 into internal arrays.

        CALL RDMEMM ( 167, cmgtag, imgdt, ier )

        IF ( ( cmgtag .eq. 'NC004002' ) .and.
     +       ( imgdt .eq. 21031713 ) .and.
     +       ( NMSUB(iunt2) .eq. 3 ) ) THEN
            print *, '        RDMEMM -> OK'
            print *, '         NMSUB -> OK'
        ELSE
            print *, '        RDMEMM -> FAILED!!'
            print *, '         NMSUB -> FAILED!!'
        END IF

C*      Read subset #18364 into internal arrays.

        CALL UFBMNS ( 18364, cmgtag, imgdt )

        IF ( ( cmgtag .eq. 'NC002003' ) .and.
     +       ( imgdt .eq. 21031900 ) .and.
     +       ( NMSUB(iunt2) .eq. 2 ) ) THEN
            print *, '        UFBMNS -> OK'
            print *, '         NMSUB -> OK'
        ELSE
            print *, '        UFBMNS -> FAILED!!'
            print *, '         NMSUB -> FAILED!!'
        END IF

	STOP
	END
