C       This is a test for NCEPLIBS-bufr.
C
C       This tests a number of library routines and functionality
C       involved in writing output BUFR messages, as noted in the
C       comments and print statements below.
C
C       Jeff Ator

#ifdef INTSIZE_8
	INTEGER*4	ISETPRM, IGETPRM
#endif

	REAL*8		r8vals ( 11, 4 ), r8bitmap ( 26 )

C*----------------------------------------------------------------------

	print *, '----------------------------------------------------'
	print *, 'testing BUFRLIB: writing OUT_3'
	print *, '  using dynamic allocation'
	print *, '  using EXITBUFR with multiple allocations'
	print *, '  using 2-22, 2-36 and 2-37 operators'
	print *, '----------------------------------------------------'

#ifdef INTSIZE_8
	CALL SETIM8B ( .true. )
#endif

C*	First message.

	IF ( ( ISETPRM ( 'NFILES', 2 ) .eq. 0 ) .and.
     +       ( ISETPRM ( 'MXMSGL', 8000 ) .eq. 0 ) ) THEN
	    print *, '        ISETPRM'
	ELSE
	    CALL BORT ( 'ISETPRM FAILURE!' )
	END IF

	CALL PKVS01 ( 'MTV', 18 )
	CALL PKVS01 ( 'USN', 2 )
	print *, '        PKVS01'

C*	Open the BUFR table and output file.

	OPEN  ( UNIT = 11, FILE = 'out3.bufr', FORM ='UNFORMATTED')
	OPEN  ( UNIT = 12, FILE = 'testfiles/OUT_3_bufrtab' )

	CALL OPENBF ( 11, 'OUT', 12 )
	print *, '        OPENBF'

	IF ( ( IGETPRM ( 'NFILES' ) .eq. 2 ) .and.
     +       ( IGETPRM ( 'MXMSGL' ) .eq. 8000 ) ) THEN
	    print *, '        IGETPRM'
	ELSE
	    CALL BORT ( 'IGETPRM FAILURE!' )
	END IF

C*	Write a standard message.

	CALL STDMSG ('Y')
	print *, '        STDMSG'

C*	Store the data values.	

	CALL OPENMB ( 11, 'FN005000', 2015030212 )
	print *, '        OPENMB'
	
	r8vals(1,1) = 2015
	r8vals(2,1) = 3
	r8vals(3,1) = 2
	r8vals(4,1) = 12
	r8vals(5,1) = 57
	r8vals(6,1) = -12.538
	r8vals(7,1) = 157.66 
	r8vals(8,1) = 20170.
	r8vals(9,1) = 37.
	r8vals(10,1) = 2.1
	r8vals(11,1) = 244.5
	CALL UFBINT ( 11, r8vals, 11, 1, nlv,
     +	  'YEAR MNTH DAYS HOUR MINU CLATH CLONH PRLC WDIR WSPD CCST' )
	print *, '        UFBINT'

	DO ii = 1, 26
	    r8bitmap(ii) = 0. 
	END DO
	r8bitmap(16) = 1.
	r8bitmap(17) = 1.
	r8bitmap(18) = 1.
	r8bitmap(21) = 1.
	CALL UFBREP ( 11, r8bitmap, 1, 26, nlv, 'DPRI' )

	r8vals(1,1) = 7.
	r8vals(2,1) = 51.
	r8vals(1,2) = 254.
	r8vals(2,2) = 1.
	r8vals(1,3) = 254.
	r8vals(2,3) = 3.
	CALL UFBREP ( 11, r8vals, 11, 3, nlv, 'GCLONG GNAP' )

	r8vals(1,1) = 97.
	r8vals(1,2) = 96.
	r8vals(1,3) = 93. 
	r8vals(1,4) = 93.
	CALL UFBREP ( 11, r8vals, 11, 4, nlv, 'PCCF' )

	r8vals(1,1) = 77.
	r8vals(1,2) = 84.
	r8vals(1,3) = 83. 
	r8vals(1,4) = 61.
	CALL UFBREP ( 11, r8vals, 11, 4, nlv, 'NCTH' )
	print *, '        UFBREP'

	CALL WRITSB ( 11 )
	print *, '        WRITSB'

	CALL EXITBUFR
	print *, '        EXITBUFR'

C*	Second message.

	IF ( ( ISETPRM ( 'NFILES', 5 ) .eq. 0 ) .and.
     +       ( ISETPRM ( 'MXMSGL', 12000 ) .eq. 0 ) ) THEN
	    print *, '        ISETPRM'
	ELSE
	    CALL BORT ( 'ISETPRM FAILURE!' )
	END IF

	CALL PKVS01 ( 'BEN', 4 )
	CALL PKVS01 ( 'MSBTI', 40 )
	CALL PKVS01 ( 'MTV', 17 )
	print *, '        PKVS01'

C*	Open the BUFR table and output file.

	OPEN  ( UNIT = 11, FILE = 'out3.bufr', FORM ='UNFORMATTED')
	OPEN  ( UNIT = 12, FILE = 'testfiles/OUT_3_bufrtab' )

	CALL OPENBF ( 11, 'APX', 12 )
	print *, '        OPENBF'

	IF ( ( IGETPRM ( 'NFILES' ) .eq. 5 ) .and.
     +       ( IGETPRM ( 'MXMSGL' ) .eq. 12000 ) ) THEN
	    print *, '        IGETPRM'
        ELSE
            CALL BORT ( 'IGETPRM FAILURE!' )
	END IF

C*	Write a standard message.

	CALL STDMSG ('Y')
	print *, '        STDMSG'

C*	Store the data values.	

	CALL OPENMB ( 11, 'FN005010', 2015030215 )
	print *, '        OPENMB'
	
	r8vals(1,1) = 2015
	r8vals(2,1) = 3
	r8vals(3,1) = 2
	r8vals(4,1) = 15
	r8vals(5,1) = 44
	r8vals(6,1) = -12.538
	r8vals(7,1) = 157.66 
	r8vals(8,1) = 19930.
	r8vals(9,1) = 305.
	r8vals(10,1) = 12.5
	r8vals(11,1) = 233.0
	CALL UFBINT ( 11, r8vals, 11, 1, nlv,
     +	  'YEAR MNTH DAYS HOUR MINU CLATH CLONH PRLC WDIR WSPD CCST' )
	print *, '        UFBINT'

	DO ii = 1, 26
	    r8bitmap(ii) = 0. 
	END DO
	r8bitmap(16) = 1.
	r8bitmap(17) = 1.
	r8bitmap(18) = 1.
	r8bitmap(26) = 1.
	CALL UFBREP ( 11, r8bitmap, 1, 26, nlv, 'DPRI' )

	r8vals(1,1) = 7.
	r8vals(2,1) = 51.
	r8vals(1,2) = 254.
	r8vals(2,2) = 1.
	r8vals(1,3) = 254.
	r8vals(2,3) = 3.
	CALL UFBREP ( 11, r8vals, 11, 3, nlv, 'GCLONG GNAP' )

	r8vals(1,1) = 92.
	r8vals(1,2) = 91.
	r8vals(1,3) = 91. 
	r8vals(1,4) = 98.
	CALL UFBREP ( 11, r8vals, 11, 4, nlv, 'PCCF' )

	r8vals(1,1) = 3.
	r8vals(1,2) = 4.
	r8vals(1,3) = 4. 
	r8vals(1,4) = 3.
	CALL UFBREP ( 11, r8vals, 11, 4, nlv, 'MAQC' )
	print *, '        UFBREP'

	CALL WRITSB ( 11 )
	print *, '        WRITSB'

	CALL CLOSBF ( 11 )
	print *, '        CLOSBF'

	STOP
	END
