C       This is a test for NCEPLIBS-bufr.
C
C       This tests a number of library routines and functionality
C       involved in writing output BUFR messages, as noted in the
C       comments and print statements below.
C
C       Jeff Ator

	REAL*8		r8ymd ( 3, 1 ),
     +			r8ltl ( 2, 1 ),
     +			r8flv ( 1, 5 ),
     +			r8oth ( 10, 1 )

#ifdef INTSIZE_8
	INTEGER*4       LCMGDF
#endif

	INTEGER		nsc(5), nrf(5), nbt(5), ierns(5)

	CHARACTER	acrn*10, libvrsn*8, tagpr*6

C*----------------------------------------------------------------------

	print *, '----------------------------------------------------'
	print *, 'testing BUFRLIB: writing OUT_1'
	print *, '  using OPENBF IO = ''OUT'' and LUNIN != LUNDX'
	print *, '  using 2-03-YYY to change reference values'
	print *, '----------------------------------------------------'

#ifdef INTSIZE_8
	CALL SETIM8B ( .true. )
#endif

	CALL BVERS ( libvrsn )
	IF ( LGT( libvrsn, '10.1.1' ) ) THEN
C*	    Specify the use of big-endian blocking.
	    print *, '        SETBLOCK'
	    CALL SETBLOCK (1)
	ENDIF

C*	Open the BUFR table and output file.

	OPEN  ( UNIT = 11, FILE = 'out1.bufr', FORM ='UNFORMATTED')
	OPEN  ( UNIT = 12, FILE = 'testfiles/OUT_1_bufrtab' )

	CALL OPENBF ( 11, 'OUT', 12 )
	print *, '        OPENBF'

C*	Write a standard, compressed BUFR message with 3 subsets.
C*      Compression will be implemented using WRITCP.

	CALL STDMSG ('Y')
	print *, '        STDMSG'

C*	First subset.

	CALL OPENMB ( 11, 'FR004029', 2012031212 )
	print *, '        OPENMB'

        IF ( LCMGDF ( 11, 'FR004029' ) .eq. 1 ) THEN
	    print *, '        LCMGDF'
        ELSE
            CALL BORT ( 'LCMGDF FAILURE!' )
	ENDIF

	CALL GETTAGPR ( 11, 'MNTH', 1, tagpr, iertgp )
	IF ( ( iertgp .eq. 0 ) .and. ( tagpr .eq. 'YYMMDD' ) ) THEN
	    print *, '        GETTAGPR'
        ELSE
            CALL BORT ( 'GETTAGPR FAILURE!' )
	ENDIF

C*	The output of the following calls will be checked below, after
C*	making additional calls to this same subroutine to verify
C*	reference values that will be modified with the 2-03 operator.

	CALL NEMSPECS ( 11, 'ACRN', 1, nsa, nra, nba, iernsa )
	CALL NEMSPECS ( 11, 'MDEVG', 1, nsm, nrm, nbm, iernsm )
	
	r8ymd(1,1) = 2012
	r8ymd(2,1) = 3
	r8ymd(3,1) = 12
	CALL UFBSEQ ( 11, r8ymd, 3, 1, nlv, 'YYMMDD' )
	r8ltl(1,1) = -35.77
	r8ltl(2,1) = 172.38
	CALL UFBSEQ ( 11, r8ltl, 2, 1, nlv, 'LTLONH' )
	print *, '        UFBSEQ'

C*	The r8flv array tests the 2-03 operator.  r8flv(1,2) contains
C*	the new reference value, which is applied to the FLVLST values
C*	in r8flv(1,3) and r8flv(1,4) when writing the message.

	r8flv(1,1) = 3500
	r8flv(1,2) = -1000
	r8flv(1,3) = 4000
	r8flv(1,4) = 5750
	r8flv(1,5) = 10722
	CALL UFBREP ( 11, r8flv, 1, 5, nlv, 'FLVLST')
	print *, '        UFBREP'

	r8oth(1,1) = 13
	r8oth(2,1) = 45
	r8oth(3,1) = 235.77
	r8oth(4,1) = 1
	r8oth(5,1) = 5.322
	r8oth(6,1) = 1
	r8oth(7,1) = 3
	r8oth(8,1) = 5
	r8oth(9,1) = 35
	r8oth(10,1) = 10.7
	CALL UFBINT ( 11, r8oth, 10, 1, nlv,
     +	    'HOUR MINU TMDB DGOT MDEVG ROLQ INTV DPOF WDIR WSPD')
	print *, '        UFBINT'

	CALL WRITCP ( 11 )
	print *, '        WRITCP'

C*	We need to run the following check after the call to WRITCP,
C*	because new reference values aren't stored into a message (nor
C*	applied when packing any other values within that message) until
C*	WRITCP calls WRITSB, which in turn calls WRTREE, which in turn
C*      calls IPKS.

	DO jj = 1, 5
	    CALL NEMSPECS ( 11, 'FLVLST', jj, nsc(jj), nrf(jj),
     +		nbt(jj), ierns(jj) )	
	END DO
	IF ( ( iernsa .eq. 0 ) .and. ( iernsm .eq. 0 ) .and.
     +		( nba .eq. 80 ) .and. ( nbm .eq. 17 ) .and.
     +		( nsm .eq. 3 ) .and. ( ierns(1) .eq. 0 ) .and.
     +		( nrf(1) .eq. -1024 ) .and. ( ierns(2) .eq. 0 ) .and.
     +		( nrf(2) .eq. -1024 ) .and. ( nbt(2) .eq. 12 ) .and.
     +		( ierns(3) .eq. 0 ) .and. ( nrf(3) .eq. -1000 ) .and.
     +		( ierns(4) .eq. 0 ) .and. ( nrf(4) .eq. -1000 ) .and.
     +		( ierns(5) .eq. 0 ) .and. ( nrf(5) .eq. -1024 ) .and.
     +		( nbt(3) .eq. 16 ) .and. ( nbt(5) .eq. 16 ) )  THEN
	    print *, '        NEMSPECS'
        ELSE
            CALL BORT ( 'NEMSPECS FAILURE!' )
	END IF

	acrn = 'TESTUPS008'
	CALL WRITLC ( 11, acrn, 'ACRN' )
	print *, '        WRITLC'

C*	Second subset.

	CALL OPENMB ( 11, 'FR004029', 2012031212 )

	CALL UFBSEQ ( 11, r8ymd, 3, 1, nlv, 'YYMMDD' )

	r8ltl(2,1) = 172.42
	CALL UFBSEQ ( 11, r8ltl, 2, 1, nlv, 'LTLONH' )

	r8flv(1,1) = 3600
	r8flv(1,4) = 5760
	r8flv(1,5) = 10730
	CALL UFBREP ( 11, r8flv, 1, 5, nlv, 'FLVLST')

	r8oth(2,1) = 48
	r8oth(3,1) = 234.69
	r8oth(5,1) = 5.001
	r8oth(8,1) = 3
	r8oth(9,1) = 30
	r8oth(10,1) = 12.2
	CALL UFBINT ( 11, r8oth, 10, 1, nlv,
     +	    'HOUR MINU TMDB DGOT MDEVG ROLQ INTV DPOF WDIR WSPD')

	CALL WRITCP ( 11 )

	acrn = 'TESTAAL225'
	CALL WRITLC ( 11, acrn, 'ACRN' )

C*	Third subset.

	CALL OPENMB ( 11, 'FR004029', 2012031212 )

	CALL UFBSEQ ( 11, r8ymd, 3, 1, nlv, 'YYMMDD' )

	r8ltl(2,1) = 172.44
	CALL UFBSEQ ( 11, r8ltl, 2, 1, nlv, 'LTLONH' )

	r8flv(1,1) = 3610
	r8flv(1,2) = -1200
	r8flv(1,4) = 5775
	r8flv(1,5) = 10730
	CALL UFBREP ( 11, r8flv, 1, 5, nlv, 'FLVLST')

	r8oth(2,1) = 51
	r8oth(3,1) = 234.11
	r8oth(5,1) = 5.012
	r8oth(8,1) = 6
	r8oth(10,1) = 12.1
	CALL UFBINT ( 11, r8oth, 10, 1, nlv,
     +	    'HOUR MINU TMDB DGOT MDEVG ROLQ INTV DPOF WDIR WSPD')

	CALL WRITCP ( 11 )

	acrn = 'TESTSWA193'
	CALL WRITLC ( 11, acrn, 'ACRN' )

	CALL CLOSBF ( 11 )
	print *, '        CLOSBF'

	STOP
	END
