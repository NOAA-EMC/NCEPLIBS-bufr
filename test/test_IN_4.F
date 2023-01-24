C       This is a test for NCEPLIBS-bufr.
C
C       This tests ???
C
C       Jeff Ator
#ifdef INTSIZE_8
	INTEGER*4	IREADSB, IUPBS01, IUPBS3, IBFMS
	INTEGER*4	MXBF, nbyt, ierr
#endif

	PARAMETER	( MXBF = 20000 )
	PARAMETER	( MXBFD4 = MXBF/4 )
	PARAMETER	( MXDS3 = 60 )

	PARAMETER	( MXR8PM = 10 )
	PARAMETER	( MXR8LV = 255 )

	REAL*8		r8arr ( MXR8PM, MXR8LV ),
     +			r8arr2 ( MXR8PM, MXR8LV )

	INTEGER		ibfmg ( MXBFD4 )

	CHARACTER	cmgtag*8, bfmg(MXBF), cds3(MXDS3)*6,
     +			tag1*8, tag2*8, tag3*8

	CHARACTER*20	filnam / 'testfiles/IN_4' /
	CHARACTER	filost / 'r' /

	EQUIVALENCE     ( bfmg (1), ibfmg (1) )

C*----------------------------------------------------------------------

	print *, '----------------------------------------------------'
	print *, 'testing BUFRLIB: reading IN_4'
	print *, '  using CRBMG with OPENBF IO = ''SEC3'''
	print *, '  using bitmap and marker operators'
	print *, '----------------------------------------------------'

#ifdef INTSIZE_8
	CALL SETIM8B ( .true. )
#endif

C*	Open the test file.

	CALL COBFL ( filnam, filost )
	print *, '         COBFL -> OK'

	CALL DATELEN  ( 10 )

	OPEN ( UNIT = 11, FILE = '/dev/null' )
	CALL OPENBF  ( 11, 'SEC3', 11 )

	print *, '        OPENBF -> OK'

	CALL MTINFO ( '../tables', 90, 91 )
	print *, '        MTINFO -> OK'

C*	Read the BUFR message from the BUFR file.

	CALL CRBMG ( bfmg, MXBF, nbyt, ierr )
	IF ( ierr .ne. 0 ) THEN
	    print *, '         CRBMG -> FAILED!!'
	ELSE
	    print *, '         CRBMG -> OK'

	    IF ( ( IUPBS01 ( ibfmg, 'MTYP' ) .eq. 5 ) .and.
     +		 ( IUPBS01 ( ibfmg, 'MTV'  ) .eq. 12 ) .and.
     +		 ( IUPBS01 ( ibfmg, 'LENM' ) .eq. 3588 ) ) THEN
		print *, '       IUPBS01 -> OK'
	    ELSE
		print *, '       IUPBS01 -> FAILED!!'
	    ENDIF

	    IF ( ( IUPBS3 ( ibfmg, 'NSUB' ) .eq. 31 ) .and.
     +		 ( IUPBS3 ( ibfmg, 'ICMP' ) .eq. 1 ) )  THEN
		print *, '        IUPBS3 -> OK'
	    ELSE
		print *, '        IUPBS3 -> FAILED!!'
	    ENDIF

	    CALL UPDS3 ( ibfmg, MXDS3, cds3, nds3 )
	    IF ( ( nds3 .eq. 51 ) .and.
     +		( cds3(1) .eq. '310023' ) .and.
     +		( cds3(5) .eq. '031031' ) .and.
     +		( cds3(32) .eq. '237000' ) .and.
     +		( cds3(44) .eq. '224255' ) ) THEN
		print *, '         UPDS3 -> OK'
	    ELSE
		print *, '         UPDS3 -> FAILED!!'
	    ENDIF

	    CALL READERME ( ibfmg, 11, cmgtag, imgdt, ierme )
	    IF ( ( ierme .eq. 0 ) .and.
     +		( cmgtag .eq. 'MSTTB001' ) ) THEN
		print *, '      READERME -> OK'
	    ELSE
		print *, '      READERME -> FAILED!!'
	    ENDIF

	    IF ( imgdt .eq. 2016041815 ) THEN
		print *, '       DATELEN -> OK'
	    ELSE
		print *, '       DATELEN -> FAILED!!'
	    ENDIF

C*	    Read the data subset from the BUFR message.

	    IF ( IREADSB (11) .ne. 0 ) THEN
		print *, '       IREADSB -> FAILED!!'
	    ELSE
		print *, '       IREADSB -> OK'

		CALL UFBINT ( 11, r8arr, MXR8PM, MXR8LV,
     +			      nr8lv, 'CLONH SAID SAZA HITE' )
		IF (  ( nr8lv .eq. 1 ) .and.
     +			( NINT(r8arr(1,1)*100000) .eq. -4246453 ).and.
     +			( NINT(r8arr(2,1)) .eq. 57 ) .and.
     +			( NINT(r8arr(3,1)*100) .eq. 5407 ) .and.
     +			( IBFMS(r8arr(4,1)) .eq. 1 ) ) THEN
		    print *, '        UFBINT -> OK'
		    print *, '         IBFMS -> OK'
		ELSE
		    print *, '        UFBINT -> FAILED!!'
		    print *, '         IBFMS -> FAILED!!'
		ENDIF

		CALL UFBREP ( 11, r8arr, MXR8PM, MXR8LV, nr8lv,
     +			      'PCCF' )
		CALL UFBREP ( 11, r8arr2, MXR8PM, MXR8LV, nr8lv2,
     +			      '224255' )
		IF (  ( nr8lv .eq. 180 ) .and.
     +			( NINT(r8arr(1,12)) .eq. 86 ) .and.
     +			( NINT(r8arr(1,15)) .eq. 38 ) .and.
     +			( NINT(r8arr(1,102)) .eq. 88 ) .and.
     +			( NINT(r8arr(1,141)) .eq. 10 ) .and.
     +		      ( nr8lv2 .eq. 72 ) .and.
     +			( NINT(r8arr2(1,12)*10) .eq. 6 ) .and.
     +			( NINT(r8arr2(1,33)*10) .eq. 4 ) ) THEN
		    print *, '        UFBREP -> OK'
		ELSE
		    print *, '        UFBREP -> FAILED!!'
		ENDIF

		CALL GETTAGRE ( 11, 'PCCF', 57, tag1, ntag1, ier1 )
		CALL GETTAGRE ( 11, 'PCCF', 154, tag2, ntag2, ier2 )
		CALL GETTAGRE ( 11, '224255', 65, tag3, ntag3, ier3 )
		IF ( ( ier1 .eq. 0 ) .and.
     +		     ( ier2 .eq. 0 ) .and.
     +		     ( ier3 .eq. 0 ) .and.
     +		    (tag1 .eq. 'TMBRST  ') .and. (ntag1 .eq. 7) .and.
     +		    (tag2 .eq. 'SPRD    ') .and. (ntag2 .eq. 4) .and.
     +		    (tag3 .eq. 'RDNE    ') .and. (ntag3 .eq. 10) ) THEN
		    print *, '      GETTAGRE -> OK'
		ELSE
		    print *, '      GETTAGRE -> FAILED!!'
		ENDIF

	    ENDIF

	ENDIF

	CALL CCBFL( )
	print *, '         CCBFL -> OK'

	STOP
	END
