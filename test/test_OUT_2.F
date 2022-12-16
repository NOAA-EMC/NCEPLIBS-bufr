#ifdef INTSIZE_8
        INTEGER*4	IGETSC
#endif

	REAL*8		r8ymd ( 3, 1 ),
     +			r8ltl ( 2, 1 ),
     +			r8oth ( 10, 1 )

	CHARACTER       libvrsn*8

	REAL*8		rpid, PKFTBV, xmiss, GETBMISS
	CHARACTER	cpid*8
	EQUIVALENCE	(rpid,cpid)

C*----------------------------------------------------------------------

	print *, '----------------------------------------------------'
	print *, 'testing BUFRLIB: writing OUT_2'
	print *, '  using OPENBF IO = ''APX'' and embedded tables'
	print *, '----------------------------------------------------'

#ifdef INTSIZE_8
	CALL SETIM8B ( .true. )
#endif

	CALL BVERS ( libvrsn )
	print *, '        BVERS'
	IF ( LGT( libvrsn, '10.1.1' ) ) THEN
C*	    Specify the use of big-endian blocking.
	    CALL SETBLOCK (1)
	    print *, '        SETBLOCK'
C*	    Modify the "missing" value.
	    xmiss = 9999.
	    CALL SETBMISS (xmiss)
	    print *, '        SETBMISS'
	ENDIF

C*	Open the BUFR table and output file.

	OPEN  ( UNIT = 11, FILE = 'out2.bufr', FORM ='UNFORMATTED')
	OPEN  ( UNIT = 12, FILE = 'testfiles/OUT_2_bufrtab' )

	CALL OPENBF ( 11, 'APX', 12 )
	print *, '        OPENBF'

	IF ( IGETSC ( 11 ) .eq. 0 ) THEN
	    print *, '        IGETSC'
        ELSE
            CALL BORT ( 'IGETSC FAILURE!' )
	ENDIF

	CALL PKVS01 ( 'OGCE', 160 )
	print *, '        PKVS01'

C*	Write an edition 4 BUFR message with 2 subsets.

	CALL PKVS01 ( 'BEN', 4 )
	print *, '        CNVED4'

C*	First subset.

	CALL OPENMB ( 11, 'NC031112', 2012101712 )
	print *, '        OPENMB'

	CALL NEMSPECS ( 11, 'TMBRST', 1, nsc, nrf, nbt, ierns )
	IF ( ( ierns .eq. 0 ) .and.
     +		( nsc .eq. 3 ) .and. ( nbt .eq. 19 ) ) THEN
	    print *, '        NEMSPECS'
        ELSE
            CALL BORT ( 'NEMSPECS FAILURE!' )
	END IF

	r8ymd(1,1) = 2012
	r8ymd(2,1) = 10 
	r8ymd(3,1) = 17
	CALL UFBINT ( 11, r8ymd, 3, 1, nlv, 'YEAR MNTH DAYS' )
	r8ltl(1,1) = -22.67
	r8ltl(2,1) = 72.02
	CALL UFBINT ( 11, r8ltl, 2, 1, nlv, 'CLATH CLONH' )

	r8oth(1,1) = 13
	r8oth(2,1) = 45
	r8oth(3,1) = 216.744
	r8oth(4,1) = 85
	r8oth(5,1) = 110
	r8oth(6,1) = 17
	r8oth(7,1) = PKFTBV(12,3) + PKFTBV(12,9)
	print *, '        PKFTBV'
	r8oth(8,1) = -0.661527
	CALL UFBINT ( 11, r8oth, 10, 1, nlv,
     +	    'HOUR MINU TMBRST SAID SACYLN ORBN OBQL SLHD1')
	print *, '        UFBINT'

	CALL WRITSB ( 11 )
	print *, '        WRITSB'

C*	Second subset.

	CALL OPENMB ( 11, 'NC031112', 2012101712 )

	CALL UFBINT ( 11, r8ymd, 3, 1, nlv, 'YEAR MNTH DAYS' )
	r8ltl(2,1) = 72.13
	CALL UFBINT ( 11, r8ltl, 2, 1, nlv, 'CLATH CLONH' )

	r8oth(2,1) = 48
	r8oth(3,1) = 214.003
	r8oth(8,1) = 0.002582
	CALL UFBINT ( 11, r8oth, 10, 1, nlv,
     +	    'HOUR MINU TMBRST SAID SACYLN ORBN OBQL SLHD1')

	cpid = 'SUBSET#2'
	CALL UFBINT ( 11, rpid, 1, 1, nlv, 'RPID' )

	IF ( NINT(xmiss) .eq. NINT(GETBMISS()) ) THEN
	    print *, '        GETBMISS'
        ELSE
            CALL BORT ( 'GETBMISS FAILURE!' )
	ENDIF

	CALL WRITSB ( 11 )

	CALL CLOSBF ( 11 )
	print *, '        CLOSBF'

	STOP
	END
