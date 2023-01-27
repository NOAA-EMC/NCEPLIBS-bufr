C       This is a test for NCEPLIBS-bufr.
C
C       This tests a number of library routines and functionality
C       involved in reading input BUFR messages, as noted in the
C       comments and print statements below.
C
C       Jeff Ator

#ifdef INTSIZE_8
	INTEGER*4	IREADSB, IUPBS01, IUPBS3, IBFMS
	INTEGER*4	MXBF, nbyt, ierr
#endif

	PARAMETER	( MXBF = 20000 )
	PARAMETER	( MXBFD4 = MXBF/4 )
	PARAMETER	( MXDS3 = 20 )

	PARAMETER	( MXR8PM = 10 )
	PARAMETER	( MXR8LV = 255 )

	REAL*8		r8arr ( MXR8PM, MXR8LV )

	INTEGER		ibfmg ( MXBFD4 )

	CHARACTER	smidstg*9, softvstg*12, cmgtag*8,
     +			bfmg(MXBF), cds3(MXDS3)*6, tagpr*8,
     +			celem(2)*60, cunit(2)*22

	CHARACTER*20	filnam / 'testfiles/IN_1' /
	CHARACTER	filost / 'r' /

	EQUIVALENCE     ( bfmg (1), ibfmg (1) )

C*----------------------------------------------------------------------

	print *, '----------------------------------------------------'
	print *, 'testing BUFRLIB: reading IN_1'
	print *, '  using CRBMG with OPENBF IO = ''SEC3'''
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

	    IF ( ( IUPBS01 ( ibfmg, 'MTYP' ) .eq. 2 ) .and.
     +		 ( IUPBS01 ( ibfmg, 'MTV'  ) .eq. 14 ) .and.
     +		 ( IUPBS01 ( ibfmg, 'LENM' ) .eq. 4169 ) ) THEN
		print *, '       IUPBS01 -> OK'
	    ELSE
		print *, '       IUPBS01 -> FAILED!!'
	    ENDIF

	    IF ( ( IUPBS3 ( ibfmg, 'NSUB' ) .eq. 1 ) .and.
     +		 ( IUPBS3 ( ibfmg, 'ICMP' ) .eq. 0 ) )  THEN
		print *, '        IUPBS3 -> OK'
	    ELSE
		print *, '        IUPBS3 -> FAILED!!'
	    ENDIF

	    CALL UPDS3 ( ibfmg, MXDS3, cds3, nds3 )
	    IF ( ( nds3 .eq. 8 ) .and.
     +		( cds3(1) .eq. '309052' ) .and.
     +		( cds3(5) .eq. '002095' ) ) THEN
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

	    CALL NEMDEFS ( 11, 'VSIGX', celem(1), cunit(1), ierndv )
	    CALL NEMDEFS ( 11, 'SMID', celem(2), cunit(2), iernds )
	    IF ( ( ierndv .eq. 0 ) .and. ( iernds .eq. 0 ) .and.
     +		( celem(1)(1:40) .eq.
     +		   'Extended vertical sounding significance ' ) .and.
     +		( celem(2)(1:39) .eq.
     +		   'Ship or mobile land station identifier ' ) .and.
     +		( cunit(1)(1:12) .eq. 'FLAG TABLE  ' ) .and.
     +		( cunit(2)(1:10) .eq. 'CCITT IA5 ' ) )  THEN

		print *, '       NEMDEFS -> OK'
	    ELSE
		print *, '       NEMDEFS -> FAILED!!'
	    ENDIF

	    IF ( imgdt .eq. 2012093012 ) THEN
		print *, '       DATELEN -> OK'
	    ELSE
		print *, '       DATELEN -> FAILED!!'
	    ENDIF

C*	    Read the data subset from the BUFR message.

	    IF ( IREADSB (11) .ne. 0 ) THEN
		print *, '       IREADSB -> FAILED!!'
	    ELSE
		print *, '       IREADSB -> OK'

		CALL GETTAGPR ( 11, 'PRLC', 192, tagpr, iertgp )
		IF ( ( iertgp .eq. 0 ) .and. ( tagpr .eq. 'WSPLRAOB' ) )
     +		  THEN
		    print *, '      GETTAGPR -> OK'
		ELSE
		    print *, '      GETTAGPR -> FAILED!!'
		ENDIF

		CALL UFBINT ( 11, r8arr, MXR8PM, MXR8LV,
     +			      nr8lv, 'CLONH A4ME HSMSL QCEVR' )
		IF (  ( nr8lv .eq. 1 ) .and.
     +			( NINT(r8arr(1,1)*100000) .eq. 10388797 ).and.
     +			( NINT(r8arr(2,1)) .eq. 7 ) .and.
     +			( NINT(r8arr(3,1)) .eq. 14 ) .and.
     +			( IBFMS(r8arr(4,1)) .eq. 1 ) ) THEN
		    print *, '        UFBINT -> OK'
		    print *, '         IBFMS -> OK'
		ELSE
		    print *, '        UFBINT -> FAILED!!'
		    print *, '         IBFMS -> FAILED!!'
		ENDIF

		CALL UFBSEQ ( 11, r8arr, MXR8PM, MXR8LV,
     +			      nr8lv, 'TDWPRAOB' )
		IF (  ( nr8lv .eq. 191 ) .and.
     +			( NINT(r8arr(8,3)*100) .eq. 29416 ) .and.
     +			( NINT(r8arr(10,11)*10) .eq. 55 ) .and.
     +			( NINT(r8arr(2,12)) .eq. 2048 ) .and.
     +			( NINT(r8arr(5,67)*100000) .eq. -1167 ) .and.
     +			( NINT(r8arr(1,186)) .eq. 2523 ) ) THEN
		    print *, '        UFBSEQ -> OK'
		ELSE
		    print *, '        UFBSEQ -> FAILED!!'
		ENDIF

		CALL READLC ( 11, smidstg, 'SMID' )
		CALL READLC ( 11, softvstg, 'SOFTV' )
		IF ( ( smidstg(7:9) .eq. 'UAO' ) .and.
     +		     ( softvstg(5:12) .eq. '5.8.5.10' ) ) THEN
		    print *, '        READLC -> OK'
		ELSE
		    print *, '        READLC -> FAILED!!'
		ENDIF

	    ENDIF

	ENDIF

	CALL CCBFL( )
	print *, '         CCBFL -> OK'

	STOP
	END
